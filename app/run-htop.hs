{-# LANGUAGE OverloadedStrings, Rank2Types, DeriveGeneric #-}

module Main where

import Control.Monad (forM)
import Conduit

import Options.Generic

import Data.TTree

import Data.Atlas.Event

-- TODO
-- at some point this all needs to be moved into library functions.
-- only basic conduits (and args?) should be left here.

data Args = Args { outfolder :: String
                 , infiles :: String
                 , xsecfile :: String
                 } deriving (Show, Generic)

instance ParseRecord Args where


main :: IO ()
          -- read in cmd line args
main = do args <- getRecord "run-hs" :: IO Args
          -- get the list of input files
          tins <- readFile (infiles args) >>= (mapM (ttree "nominal") . lines)

          ns <- forM tins $ \t -> project t =$= mapMC (print :: Event -> IO ()) $$ lengthC
          print (sum ns :: Int)

          -- project the samples onto the nominal histos (in parallel)
          -- samps <- sequence . withStrategy (parList rseq) . map (\fn -> runResourceT (yield (fn <> "\n") $$ stderrC) >> project) $ tins
          return ()

{-
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
