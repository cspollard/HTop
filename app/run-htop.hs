{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}

module Main where

import           Atlas
import           BFrag.Event
import           Control.Arrow             (first)
import qualified Control.Foldl             as F
import           Control.Monad.Trans.Maybe
import           Data.Functor.Identity
import           Data.List                 (nub)
import qualified Data.Map.Strict           as M
import           Data.Semigroup
import qualified Data.Text                 as T
import           Data.TFile
import           Data.TTree
import           Data.Typeable
import           GHC.Float
import           Options.Generic
import           Pipes
import qualified Pipes.Prelude             as P
import           System.IO                 (BufferMode (..), hSetBuffering,
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

  mapM_ (fillFile treeSysts) fns


fillFile
  :: (MonadIO m, MonadCatch m)
  => [String]
  -- -> Maybe (Int, Sum Double, Folder (Vars YodaObj))
  -> String
  -- -> m (Maybe (Int, Sum Double, Folder (Vars YodaObj)))
  -> m ()
fillFile systs fn = do
  liftIO . putStrLn $ "analyzing file " <> fn

  -- check whether or not this is a data file
  tfile <- tfileOpen fn
  liftIO . putStrLn $ "checking sumWeights"

  tw <- ttree tfile "sumWeights"

  -- partial.
  -- throw an error when there is no dsid.
  (Just (dsidc :: CInt)) <-
    P.head $ runTTree (readBranch "dsid") tw

  let dsid = fromEnum dsidc
      fo = F.Fold (+) (0 :: Float) id

  liftIO . putStrLn $ "dsid: " ++ show dsid

  sow <-
    fmap (Sum . float2Double)
    . F.purely P.fold fo
    $ runTTree (readBranch "totalEventsWeighted") tw

  liftIO . putStrLn $ "sum of weights: " ++ show (getSum sow)

  let entryFold =
        F.purely P.fold
        $ F.Fold (\m (i, j) -> M.insert i j m) M.empty id

      entryRead = (,) <$> readRunEventNumber <*> readEntry
      entries t = entryFold $ runTTree entryRead t

  trueTree <- ttree tfile "particleLevel"
  nomTree <- ttree tfile "nominal"

  trueEntries <- entries trueTree
  nomEntries <- entries nomTree

  (systTrees :: M.Map String TTree) <-
    M.fromList <$> mapM (\tn -> (tn,) <$> ttree tfile tn) systs

  systEntries <- mapM entries systTrees

  let allEntries :: [(CUInt, CULong)]
      allEntries = nub $
        M.keys nomEntries ++ M.keys trueEntries
        ++ M.keys (M.foldr M.union M.empty systEntries)

  runEffect
    $ each allEntries
      >-> P.map (\x -> (x, lookup' trueEntries nomEntries systEntries x))
      >-> readEvents trueTree nomTree systTrees
      >-> P.print


  where
    lookup'
      :: Ord a
      => M.Map a b
      -> M.Map a b
      -> M.Map k (M.Map a b)
      -> a
      -> (Maybe b, Maybe b, M.Map k (Maybe b))
    lookup' trueMap nomMap systMaps i =
      ( M.lookup i trueMap
      , M.lookup i nomMap
      , fmap (M.lookup i) systMaps
      )


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
  liftIO $ print isysts
  let f tr t (Just i) = lift $ readTree tr i t
      f _ t Nothing   = return (fail "Nothing", t)

      systs' =
        M.mergeWithKey
          (\_ t mi -> Just $ f (readRecoEvent $ MC' NoVars) t mi)
          (const . M.singleton "" $ throwM TreeReadError)
          (const . M.singleton "" $ throwM TreeReadError)
          ttsysts
          isysts

  (true, tttrue') <- f readTrueEvent tttrue mitrue
  (nom, ttnom') <- f (readRecoEvent $ MC' AllVars) ttnom minom
  msysts <- sequence systs'

  -- TODO
  -- traverses Map twice
  let systs = fst <$> msysts
      ttsysts' = snd <$> msysts

  -- TODO
  -- make an Event out of what we've got
  yield . Event rn en true . toEvent nom $ M.mapKeys T.pack systs
  readEvents tttrue' ttnom' ttsysts'

  where

    toEvent :: forall a. PhysObj a -> M.Map T.Text (PhysObj a) -> PhysObj a
    toEvent n s =
      let systs :: M.Map Text (Maybe (a, Double))
          systs = runIdentity . getNominal . runPhysObj <$> s
          n' :: VarsT Identity (Maybe (a, Double))
          n' = runPhysObj n
          v :: VarsT Identity (Maybe (a, Double))
          v = variations (pure . mappend (strictMap systs)) n'
      in PhysObjT . WriterT . MaybeT $ (fmap.fmap.fmap) (sf "wgt") v


combineTrees
  :: (MonadThrow m, MonadIO m)
  => (TTree, TreeRead m a)
  -> (TTree, TreeRead m b)
  -> (Maybe a -> Maybe b -> c)
  -> Pipe (Maybe Int, Maybe Int) c m r
combineTrees (ta, tra) (tb, trb) f = do
  (mi, mj) <- await

  (ma, ta') <-
    case mi of
      Just i  -> lift $ first Just <$> readTree tra i ta
      Nothing -> return (Nothing, ta)

  (mb, tb') <-
    case mj of
      Just j  -> lift $ first Just <$> readTree trb j tb
      Nothing -> return (Nothing, tb)

  yield $ f ma mb
  combineTrees (ta', tra) (tb', trb) f


--       readEntries tn = do
--         t <- ttree tfile tn
--
--         -- deal with possible missing trees
--         nt <- isNullTree t
--         when nt $
--           liftIO . putStrLn $ "missing tree " <> tn <> " in file " <> fn <> "."
--
--         entryProd <- each <$> if nt then return S.empty else entries t
--
--         let l = produceTTree (tr $ dmc dsid tn) t (entryProd >-> P.map snd)
--
--         return (T.pack tn, l)
--
--   truthTree <- snd <$> treeProd readTrue "particleLevel"
--
--
--   recoTrees <-
--     recoVariations . strictMap . M.fromList
--     <$> mapM (treeProd readReco) (nom : systs)
--
--   let eventProd = alignThesePipes fst fst truthTree recoTrees >-> P.map toEvent
--
--       toEvent ((i, j), k) =
--         Event i j
--         . over here snd
--         . over there snd
--         $ k
--
--   F.impurely P.foldM eventHs eventProd
--   -- runEffect $ for eventProd (liftIO . print)
--
--   liftIO . putStrLn $ "closing file " <> fn
--   liftIO $ tfileClose tfile
--
--
-- transposeM
--   :: forall k k' a. (Ord k, Ord k')
--   => M.Map k (M.Map k' a) -> M.Map k' (M.Map k a)
-- transposeM = M.foldrWithKey f M.empty
--   where
--     f :: k -> M.Map k' a -> M.Map k' (M.Map k a) -> M.Map k' (M.Map k a)
--     f k inmap outmap = M.unionWith (<>) outmap $ M.singleton k <$> inmap
