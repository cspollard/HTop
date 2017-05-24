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
import           Control.Arrow   (first)
import qualified Control.Foldl   as F
import           Control.Lens    hiding (each)
import           Control.Monad   (when)
import           Data.Align
import qualified Data.Map.Strict as M
import           Data.Semigroup
import qualified Data.Set        as S
import qualified Data.Text       as T
import           Data.TFile
import           Data.These
import           Data.TTree
import           GHC.Float
import           Options.Generic
import           Pipes
import           Pipes.Core
import qualified Pipes.Prelude   as P
import           System.IO       (BufferMode (..), hSetBuffering, stdout)


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

      dmc ds tn =
        if ds == 0
          then Data'
          else MC' $
            if ds == 410501 && tn == "nominal"
              then AllVars
              else NoVars

  treesl <- mapM (\tn -> (tn,) <$> ttree tfile tn) systs
  treeEntries <-
    traverse (\(tn, t) -> fmap (M.singleton tn) <$> entries t) treesl
  let -- xs :: M.Map (CUInt, CULong) (M.Map String Int)
      xs = M.unionsWith M.union treeEntries
      -- trees :: M.Map String TTree
      trees = M.fromList treesl


  runEffect
    $ each xs
      >-> runTrees (readRecoEvent $ MC' NoVars) trees
      -- >-> P.map ((fmap.fmap.fmap) $ fmap (view toPtEtaPhiE) . view jets)
      -- >-> P.map ((fmap.fmap) runPhysObj)
      >-> P.print


runTrees
  :: (MonadThrow m, MonadIO m, Ord k)
  => TreeRead m a
  -> M.Map k TTree
  -> Pipe (M.Map k Int) (M.Map k (Maybe a)) m r
runTrees tr ts = do
  is <- await
  let f _ t i = Just $ first Just <$> readTree tr i t
      xs = M.mergeWithKey f (fmap (return . (Nothing,))) (const M.empty) ts is

  m <- lift $ sequence xs
  yield $ fst <$> m
  runTrees tr $ snd <$> m



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
