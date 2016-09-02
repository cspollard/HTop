{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Monad (forM, when)
import Control.Applicative (liftA2, getZipList)
import Conduit

import qualified Data.Text as T

import Options.Generic

import Data.TTree
import Data.Atlas.Histograms
import Data.Atlas.Sample
import Data.Atlas.Event
import Data.Histogram.Extra

data Args = Args { outfolder :: String
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

printEvent :: Int -> Event -> IO ()
printEvent i x = putStrLn ("event " ++ show i ++ ":") >> print x

main :: IO ()
          -- read in cmd line args
main = do args <- getRecord "run-hs" :: IO Args
          -- get the list of input trees
          fs <- lines <$> readFile (infiles args)

          -- get sumweights
          sws <- mapM (ttree "sumWeights") fs
          sumweights <- fmap sum . forM sws $ \s -> project s =$= mapC totalEventsWeighted $$ sumC
          print sumweights

          -- project trees
          tins <- mapM (ttree "nominal") fs
          (ns, hs) <- fmap unzip $ forM tins $ \t -> project t =$= everyC 10000 printEvent =$= mapC (1.0,) $$ channelHistos
          putStrLn (show (sum ns) ++ " events analyzed.") 

          let hs' = foldl1 (liftA2 haddYH) hs
          runResourceT $ yieldMany (getZipList hs') =$= mapC (T.unpack . showHisto) $$ sinkFile (outfolder args ++ '/' : "test.yoda")

{-
          -- project the samples onto the nominal histos (in parallel)
          samps <- sequence . withStrategy (parList rseq) . map (\fn -> runResourceT (yield (fn <> "\n") $$ stderrC) >> project) $ tins

          let m = M.fromListWith (liftA2 haddYH) $ map (first $ ordedBy dsid) samps

          -- read in the sample cross sections
          xsecs <- runResourceT $ sourceFile (xsecfile args) $$ sinkParser crossSectionInfo

          let scaledHists = flip M.mapWithKey m $
                                \(Orded ds s) hs -> case ds of
                                                         0 -> hs
                                                         _ -> over (traverse . yhHisto) (flip scaleBy $ (xsecs IM.! ds) * 3210 / sumWeights s) hs

          let mergedHists = M.mapKeysWith (liftA2 haddYH) (processTitle . orded) scaledHists & fmap getZipList

          let outfname = outfolder args
          createDirectoryIfMissing True outfname
        
          forM_ (M.toList mergedHists) (\(fout, hs) -> runResourceT $ mapM_ yield hs $$ CL.map (T.encodeUtf8 . showHisto) =$= sinkFile (outfname ++ '/' : T.unpack fout ++ ".yoda")) 

          runResourceT $ CC.sourcePut (put mergedHists) =$= gzip $$ sinkFile (outfname ++ "hists.gz")

    where mcWs = ["weight_mc", "weight_pileup"]
-}
