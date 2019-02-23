{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
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
import           BFrag.Model
import           Control.Applicative        (empty)
import qualified Control.Foldl              as F
import           Control.Lens               hiding (each)
import           Control.Monad              (when)
import           Control.Monad.Fail
import           Control.Monad.State.Strict
import           Control.Monad.Writer.Class (writer)
import           Data.Bifunctor             (first)
import           Data.List                  (isInfixOf, nub)
import qualified Data.Map.Strict            as M
import           Data.Maybe                 (fromMaybe)
import           Data.Semigroup
import qualified Data.Text                  as T
import           Data.TFile
import           Data.TTree
import           Data.Typeable
import           GHC.Exts                   (IsList (..))
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
    , nevents :: Maybe Int
    } deriving (Show, Generic)

instance ParseRecord Args where

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering

  -- read in cmd line args
  args <- getRecord "run-hs" :: IO Args

  -- get the list of input trees
  fns <- filter (not . null) . lines <$> readFile (infiles args)

  let combF (Just (procinfo, sow, hs)) f = do
        (procinfo', sow', hs') <- fillFile treeSysts f Nothing
        let hs'' = mappend hs hs'
        seq hs'' . return $ if procinfo == procinfo'
          then Just (procinfo, sow+sow', hs'')
          else Nothing
      combF Nothing f = Just <$> fillFile treeSysts f (nevents args)

      foldFiles = F.FoldM combF (return Nothing) return

  hs <- F.impurely P.foldM foldFiles $ each fns

  putStrLn ("writing to file " ++ outfile args)
  views _Just (encodeFile $ outfile args) hs


-- TODO
-- bracket around file
fillFile
  :: (MonadIO m, MonadCatch m, MonadFail m)
  => [String]
  -> String
  -> Maybe Int
  -> m (ProcessInfo, Sum Double, Folder (Annotated (Vars Obj)))
fillFile systs fn nevt = do
  liftIO . putStrLn $ "analyzing file " <> fn

  -- check whether or not this is a data file
  tfile <- tfileOpen fn
  liftIO . putStrLn $ "checking sumWeights"

  tw <- ttree tfile "sumWeights"

  -- partial.
  -- throw an error when there is no dsid.
  (Just (dsidc :: CInt)) <-
    P.head . evalStateP tw $ yield 0 >-> pipeTTree (readBranch "dsid")

  let dsid' = fromEnum dsidc
      procinfo =
        ProcessInfo dsid'
        $ if "_a" `isInfixOf` fn
          then AFII
          else if dsid' == 0 then DS else FS


  liftIO . putStrLn $ "procinfo: " ++ show procinfo

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

  nomTree <- ttree tfile "nominal"
  nomEntries <- entries nomTree

  trueTree <- ttree tfile "particleLevel"
  trueEntries <-
    if dsid' == 0
      then return mempty
      else entries trueTree

  let systflag =
        case dsid' of
          0 -> Data'
          _ ->
            if procinfo `elem` ([nomkey] ++ zjetskeys ++ stopdrkeys ++ dibosonkeys)
              then MC' AllVars
              else MC' NoVars
      take' = maybe cat P.take nevt

  (systTrees :: M.Map String TTree) <-
    case systflag of
      Data' -> return mempty
      MC' NoVars -> return mempty
      MC' AllVars -> M.fromList <$> mapM (\tn -> (tn,) <$> ttree tfile tn) systs

  systEntries <- mapM entries systTrees

  let allEntries = entryMap trueEntries nomEntries systEntries

  hs <-
    F.purely P.fold eventHs
    $ each allEntries
      >-> take'
      >-> doEvery 100
          ( \i _ -> liftIO . putStrLn $ show i ++ " entries processed." )
      >-> readEvents systflag trueTree nomTree systTrees
      >-> P.map return

  liftIO . putStrLn $ "closing file " <> fn
  liftIO $ tfileClose tfile
  return (procinfo, Sum sow, hs)

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
  => DataMC'
  -> TTree
  -> TTree
  -> M.Map String TTree
  -> Pipe ((CUInt, CULong), (Maybe Int, Maybe Int, M.Map String (Maybe Int))) Event m r
readEvents systflag tttrue ttnom ttsysts = do
  ((rn, en), (mitrue, minom, isysts)) <- await

  -- TODO
  -- fugly
  -- this attempts to read an entry from a tree and returns "empty" if it's not
  -- there
  let f tr t (Just i) =
        fmap (first Just)
        . flip runStateT t
        $ fromMaybe (error "unable to read event from ttree!")
          <$> readTTreeEntry tr i
      f _ t Nothing   = return (Nothing, t)

      g tr t (Just i) =
        flip runStateT t
        $ fromMaybe (error "unable to read event from ttree!")
          <$> readTTreeEntry tr i
      g _ t Nothing   = return (empty, t)

  (true, tttrue') <- f readTrueEvent tttrue mitrue

  let bhs :: [BHadron]
      bhs =
        case true of
          Nothing -> []
          Just (trueEvt, _) ->
            toListOf (trueJets.traverse.tjBHadrons.traverse) trueEvt

      systs' =
        M.mergeWithKey
          (\_ t mi -> Just $ g (readRecoEvent (MC' NoVars) bhs) t mi)
          (fmap $ const $ throwM TreeReadError)
          (fmap $ const $ throwM TreeReadError)
          ttsysts
          isysts

  (nom, ttnom') <- g (readRecoEvent systflag bhs) ttnom minom
  msysts <- sequence systs'

  -- TODO
  -- traverses Map twice
  let systs = fst <$> msysts
      ttsysts' = snd <$> msysts

  yield
    . Event rn en (maybe empty writer true)
    . toEvent nom
    . fromList
    . fmap (first T.pack)
    $ M.toList systs

  readEvents systflag tttrue' ttnom' ttsysts'

  where
    toEvent :: PhysObj a -> StrictMap T.Text (PhysObj a) -> PhysObj a
    toEvent n s =
      let systs = view nominal . runPhysObj <$> s
          n' = runPhysObj n
          n'' = n' & variations %~ mappend systs
      in physObj n''
