{-# LANGUAGE OverloadedStrings, Rank2Types, DeriveGeneric #-}

module Main where

import Control.Lens

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Data.Histogram.Extra

import Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Cereal as CC
import qualified Data.Conduit.List as CL

import Data.Serialize (put)

import Data.Conduit.Zlib (ungzip, gzip)
import Data.Conduit.Attoparsec

import Control.Monad (forM_)
import Control.Applicative (liftA2, getZipList)

import Data.Semigroup

import System.Directory

import Data.Atlas.Tree
import Data.Atlas.Sample
import Data.Atlas.Histograms
import Data.Atlas.CrossSections
import Data.Atlas.ProcessInfo

import Control.Parallel.Strategies (withStrategy, parList, rseq)

import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM

import Control.Arrow (first)

import Options.Generic
import Data.Orded

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
main = do args <- getRecord "jsonToYoda" :: IO Args

          -- get the list of input files
          fins <- runResourceT $ sourceFile (infiles args) =$= CB.lines =$= CL.map (T.unpack . T.decodeUtf8) $$ CL.consume

          -- project the samples onto the nominal histos (in parallel)
          samps <- sequence . withStrategy (parList rseq) . map (\fn -> runResourceT (yield (fn <> "\n") $$ stderrC) >> runResourceT (sourceFile fn =$= ungzip $$ project mcWs channelHistos)) $ fins

          let m = M.fromListWith (liftA2 haddYH) $ map (first $ ordedBy dsid) samps

          -- read in the sample cross sections
          xsecs <- runResourceT $ sourceFile (xsecfile args) $$ sinkParser crossSectionInfo

          let scaledHists = flip M.mapWithKey m $
                                (\(Orded ds s) hs -> case ds of
                                                         0 -> hs
                                                         _ -> over (traverse . yhHisto) (flip scaleBy $ (xsecs IM.! ds) * 3210 / sumWeights s) hs)

          let mergedHists = M.mapKeysWith (liftA2 haddYH) (processTitle . orded) scaledHists & fmap getZipList

          let outfname = outfolder args
          createDirectoryIfMissing True outfname
        
          forM_ (M.toList mergedHists) (\(fout, hs) -> runResourceT $ mapM_ yield hs $$ CL.map (T.encodeUtf8 . showHisto) =$= sinkFile (outfname ++ '/' : (T.unpack fout) ++ ".yoda")) 

          let forOutput (YodaHisto p xl yl h) = ([p, xl, yl], h)
          runResourceT $ CC.sourcePut (put $ (fmap . fmap) forOutput mergedHists) =$= gzip $$ sinkFile (outfname ++ "hists.gz")

    where mcWs = ["weight_mc", "weight_pileup"]
