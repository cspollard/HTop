{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}

module Main where

import           Codec.Compression.GZip (compress)
import qualified Control.Foldl          as F
import           Control.Lens
import           Control.Monad          (when)
import qualified Data.ByteString.Lazy   as BS
import qualified Data.Map.Strict        as M
import           Data.Semigroup
import           Data.Serialize         (encodeLazy)
import qualified Data.Text              as T
import           Debug.Trace
import           GHC.Float
import qualified List.Transformer       as L
import           Options.Generic
import           System.IO              (BufferMode (..), hSetBuffering, stdout)

import           Atlas
import           BFrag.Event
import           Data.TFile
import           Data.TTree


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

  let fnl = L.select fns :: L.ListT IO String
      f = F.FoldM
            (fillFile ("nominal", treeSysts))
            (return Nothing)
            return

  imh <- F.impurely L.foldM f fnl

  putStrLn ("writing to file " ++ outfile args)

  BS.writeFile (outfile args) (compress . encodeLazy $ imh)


fillFile
  :: (String, [String])
  -> Maybe (Int, Double, Folder (Vars YodaObj))
  -> String
  -> IO (Maybe (Int, Double, Folder (Vars YodaObj)))
fillFile (nom, systs) m fn = do
  putStrLn $ "analyzing file " <> fn

  -- check whether or not this is a data file
  f <- tfileOpen fn
  tw <- ttree f "sumWeights"
  (L.Cons (dsidc :: CInt) _) <- L.next $ runTTreeL (readBranch "dsid") tw

  let dsid = fromEnum dsidc
      fo = F.Fold (+) (0 :: Float) id

  sow <-
    fmap float2Double
      . F.purely L.fold fo
      $ runTTreeL (readBranch "totalEventsWeighted") tw

  let loopTree tn = do
        t <- ttree f tn
        putStrLn $ "looping over tree " <> tn

        -- deal with possible missing trees
        nt <- isNullTree t
        when nt $ do
          putStrLn $ "missing tree " <> tn <> " in file " <> fn <> "."
          putStrLn "continuing."

        let l = if nt then L.empty else runTTreeL tmp t
            tmp = readEvent (dsid == 0)

        fmap (tn,) $! F.purely L.fold eventHs l

  systHs :: Folder (M.Map T.Text YodaObj) <-
    Folder
    -- don't run syst trees for data...
    . (if dsid == 0 then const M.empty else id)
    . transposeM
    . M.fromList
    -- . fmap (\x -> traceShow (fst x) x)
    . fmap (over _1 T.pack . fmap (folderToMap . fmap (view nominal)))
    <$> mapM loopTree systs

  nomHs <- snd <$> loopTree nom
  let hs =
        inF2
          (M.intersectionWith (\n v -> over variations (<> v) n))
          nomHs
          systHs

  putStrLn $ "closing file " <> fn
  tfileClose f

  case m of
      Nothing ->
        hs `seq` return (Just (dsid, sow, hs))
      Just (dsid', n, hs') -> do
        when (dsid /= dsid')
          $ error "attempting to analyze different dsids in one run!!!"
        let n' = n+sow
            sm' = mappend hs hs'
        n' `seq` sm' `seq` return (Just (dsid, n', sm'))


transposeM
  :: forall k k' a. (Ord k, Ord k')
  => M.Map k (M.Map k' a) -> M.Map k' (M.Map k a)
transposeM = M.foldrWithKey f M.empty
  where
    f :: k -> M.Map k' a -> M.Map k' (M.Map k a) -> M.Map k' (M.Map k a)
    f k inmap outmap = M.unionWith (<>) outmap $ M.singleton k <$> inmap


  -- -- [(
  -- . concat
  -- -- [[(k, (k', a))]]
  -- . fmap sequenceA
  -- -- [(k, [(k', a)])]
  -- . M.toList
  -- -- Map k ([k', a])
  -- $ M.toList <$> m
