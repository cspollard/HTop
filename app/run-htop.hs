{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import           Atlas
import           BFrag.Event
import qualified Control.Foldl              as F
import           Control.Lens               hiding (each)
import           Control.Monad              (when)
import qualified Control.Monad.Fail         as MF
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Maybe
import           Data.List                  (nub)
import qualified Data.Map.Strict            as M
import           Data.Maybe                 (fromMaybe)
import           Data.Semigroup
import qualified Data.Text                  as T
import           Data.TFile
import           Data.TTree
import           Data.Typeable
import           GHC.Float
import           Options.Generic
import           Pipes
import           Pipes.Lift
import qualified Pipes.Prelude              as P
import           System.IO                  (BufferMode (..), hSetBuffering,
                                             stdout)


-- TODO
-- we should use the comonad instance of Fold to remove the ~doubling of
-- memory usage on >1 file.

data Args =
  Args
    { outfile :: String
    , infiles :: String
    } deriving (Show, Generic)

instance ParseRecord Args where

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering

  -- read in cmd line args
  args <- getRecord "run-hs" :: IO Args

  -- get the list of input trees
  fns <- filter (not . null) . lines <$> readFile (infiles args)

  let combF (Just (dsid, sow, hs)) f = do
        (dsid', sow', hs') <- fillFile treeSysts f
        let hs'' = mappend hs hs'
        seq hs'' . return $ if dsid == dsid'
          then Just (dsid, sow+sow', hs'')
          else Nothing
      combF Nothing f = Just <$> fillFile treeSysts f

      foldFiles = F.FoldM combF (return Nothing) return

  hs <- F.impurely P.foldM foldFiles $ each fns

  putStrLn ("writing to file " ++ outfile args)
  views _Just (encodeFile $ outfile args) hs


-- TODO
-- bracket
fillFile
  :: (MonadIO m, MonadCatch m)
  => [String]
  -> String
  -> m (Int, Sum Double, Folder (Vars YodaObj))
fillFile systs fn = do
  liftIO . putStrLn $ "analyzing file " <> fn

  -- check whether or not this is a data file
  tfile <- tfileOpen fn
  liftIO . putStrLn $ "checking sumWeights"

  tw <- ttree tfile "sumWeights"

  -- partial.
  -- throw an error when there is no dsid.
  (Just (dsidc :: CInt)) <-
    P.head . evalStateP tw $ yield 0 >-> pipeTTree (readBranch "dsid")

  let dsid = fromEnum dsidc

  liftIO . putStrLn $ "dsid: " ++ show dsid

  sow <-
    fmap float2Double
    . F.purely P.fold F.sum
    . evalStateP tw
    $ each ([0..] :: [Int]) >-> pipeTTree (readBranch "totalEventsWeighted")

  liftIO . putStrLn $ "sum of weights: " ++ show sow

  let entryFold =
        F.purely P.fold
        $ F.Fold (\m (i, j) -> M.insert i j m) M.empty id

      entryRead = (,) <$> readRunEventNumber <*> readEntry
      entries t = entryFold . evalStateP t $ allIdxs >-> pipeTTree entryRead
      allIdxs = each ([0..] :: [Int])

  trueTree <- ttree tfile "particleLevel"
  nomTree <- ttree tfile "nominal"

  -- TODO
  -- not running over truth atm
  trueEntries <- entries trueTree
  nomEntries <- entries nomTree

  (systTrees :: M.Map String TTree) <-
    M.fromList <$> mapM (\tn -> (tn,) <$> ttree tfile tn) systs

  systEntries <- mapM entries systTrees

  let allEntries = entryMap trueEntries nomEntries systEntries

  hs <-
    F.purely P.fold eventHs
    $ each allEntries
      >-> readEvents trueTree nomTree systTrees
      >-> P.map return
      >-> doEvery 100
          (\i _ -> liftIO . putStrLn $ show i ++ " entries processed.")

  liftIO . putStrLn $ "closing file " <> fn
  liftIO $ tfileClose tfile
  return (dsid, Sum sow, hs)

  where
    entryMap
      :: Ord a
      => M.Map a b
      -> M.Map a b
      -> M.Map k (M.Map a b)
      -> [(a, (Maybe b, Maybe b, M.Map k (Maybe b)))]
    entryMap trueMap nomMap systMaps =
      fmap (\i -> (i, lookup' i)) . nub
      $ M.keys trueMap ++ M.keys nomMap ++ foldMap M.keys systMaps
      where
        lookup' i =
          ( M.lookup i trueMap
          , M.lookup i nomMap
          , fmap (M.lookup i) systMaps
          )


doEvery :: Monad m => Int -> (Int -> a -> m ()) -> Pipe a a m r
doEvery m f = go 0
  where
    go n = do
      x <- await
      when ((n `mod` m) == 0) (lift $ f n x)
      yield x
      go (n+1)


data TreeReadError = TreeReadError deriving (Typeable, Show)
instance Exception TreeReadError


readEvents
  :: (MonadThrow m, MonadIO m)
  => TTree
  -> TTree
  -> M.Map String TTree
  -> Pipe ((CUInt, CULong), (Maybe Int, Maybe Int, M.Map String (Maybe Int))) Event m r
readEvents tttrue ttnom ttsysts = do
  ((rn, en), (mitrue, minom, isysts)) <- await

  let f tr t (Just i) = flip runStateT t $ mf <$> readTTreeEntry tr i
      f _ t Nothing   = return (MF.fail "Nothing", t)

      mf = fromMaybe (MF.fail "Nothing")


      systs' =
        M.mergeWithKey
          (\_ t mi -> Just $ f (readRecoEvent $ MC' NoVars) t mi)
          (fmap $ const $ throwM TreeReadError)
          (fmap $ const $ throwM TreeReadError)
          ttsysts
          isysts

  (true, tttrue') <- f readTrueEvent tttrue mitrue
  (nom, ttnom') <- f (readRecoEvent $ MC' AllVars) ttnom minom
  msysts <- sequence systs'

  -- TODO
  -- traverses Map twice
  let systs = fst <$> msysts
      ttsysts' = snd <$> msysts

  yield
    . Event rn en true
    . toEvent nom
    $ M.mapKeys T.pack systs

  readEvents tttrue' ttnom' ttsysts'

  where
    toEvent :: forall a. PhysObj a -> M.Map T.Text (PhysObj a) -> PhysObj a
    toEvent n s =
      let systs = view nominal . runPhysObj' <$> s
          n' = runPhysObj' n
          n'' = over variations (mappend $ strictMap systs) n'
      in PhysObj . WriterT $ MaybeT n''
