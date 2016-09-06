{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Lens

import Conduit
import Data.Conduit.Binary (sourceLbs)
import Data.Conduit.Zlib (gzip)
import Data.Serialize (encodeLazy)
import Data.Serialize.ZipList

import Control.Monad (forM, forM_, when)
import Control.Applicative (liftA2, getZipList)

import qualified Data.Text as T

import Options.Generic

import qualified Data.IntMap.Strict as IM

import Data.TTree
import Data.Atlas.Histograms
import Data.Atlas.Sample
import Data.Atlas.Event
import Data.YODA.Histo
import Data.Atlas.CrossSections

data Args = Args { outfile :: String
                 , infiles :: String
                 , xsecfile :: String
                 } deriving (Show, Generic)

instance ParseRecord Args where

everyC :: Monad m => Int -> (Int -> a -> m ()) -> Conduit a m a
everyC n f = loop 0
    where loop i = do mx <- await
                      case mx of
                           Just x -> when (i `mod` n == 0) (lift $ f i x) >> yield x >> loop (i+1)
                           Nothing -> return ()

main :: IO ()
          -- read in cmd line args
main = do args <- getRecord "run-hs" :: IO Args

          xsecs <- readXSecFile (xsecfile args)

          -- get the list of input trees
          fs <- lines <$> readFile (infiles args)

          samps <- forM fs $ \f -> do putStrLn $ "analyzing events in file " ++ f
                                      wt <- ttree "sumWeights" f
                                      tt <- ttree "nominal" f
                                      s <- foldl1 addSampInfo <$> (project wt $$ sinkList)
                                      (n, h) <- project tt $$ mapC (1.0,) =$= channelHistos
                                      putStrLn $ show n ++ " events analyzed.\n"
                                      return (s, h)

          let m = IM.fromListWith (\(s, h) (s', h') -> (addSampInfo s s', liftA2 addYH h h')) $ map ((,) <$> fromEnum . dsid . fst <*> id) samps


          let scaledHists = flip IM.mapWithKey m $
                                \ds (s, hs) -> case ds of
                                                    0 -> hs
                                                    _ -> over (traverse . thing) (flip scaledBy $ (xsecs IM.! ds) / totalEventsWeighted s) hs


          runResourceT $ sourceLbs (encodeLazy scaledHists) =$= gzip $$ sinkFile (outfile args)

{-

          forM_ (IM.toList scaledHists) $ \(ds, hs) ->
                                            runResourceT $ yieldMany (getZipList hs) =$= mapC (T.unpack . showHisto) $$ sinkFile

          let mergedHists = M.mapKeysWith (liftA2 haddYH) (processTitle . orded) scaledHists & fmap getZipList

          let outfname = outfolder args
          createDirectoryIfMissing True outfname
        
          forM_ (M.toList mergedHists) (\(fout, hs) -> runResourceT $ mapM_ yield hs $$ CL.map (T.encodeUtf8 . showHisto) =$= sinkFile (outfname ++ '/' : T.unpack fout ++ ".yoda")) 

          runResourceT $ CC.sourcePut (put mergedHists) =$= gzip $$ sinkFile (outfname ++ "hists.gz")

    where mcWs = ["weight_mc", "weight_pileup"]
-}
