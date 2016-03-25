{-# LANGUAGE OverloadedStrings, Rank2Types, DeriveGeneric #-}

module Main where

import Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Data.HEP.YodaHisto
import Data.Histogram

import qualified Data.Conduit.Binary as CB

import Conduit

import Data.Conduit.Zlib (ungzip)
import Data.Conduit.Attoparsec
import qualified Data.Conduit.List as CL

import Control.Monad (forM_)

import Data.Semigroup

import System.Directory

import Data.Atlas.Tree
import Data.Atlas.Sample
import Data.Atlas.TopTree
import Data.Atlas.Histograms
import Data.Atlas.CrossSections
import Data.Atlas.ProcessInfo

import Control.Parallel.Strategies (withStrategy, parList, rseq)

import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM

import Control.Arrow ((&&&))

import Options.Generic
import Data.SGList

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

          -- read in the sample cross sectinos
          xsecs <- runResourceT $ sourceFile (xsecfile args) $$ sinkParser crossSectionInfo

          -- project the samples onto the nominal histos (in parallel)
          samps <- sequence . withStrategy (parList rseq) . map (\fn -> runResourceT (yield (fn <> "\n") $$ stderrC) >> runResourceT (sourceFile fn =$= ungzip $$ project nominalHistos)) $ fins

          let m = M.fromListWith (<>) $ map ((dsid . fst) &&& id) (samps :: [Sample (SGList YodaHisto1D)])

          let scaledHists = fmap (\ss@(s, _) -> freezeSample ss `scaleW` (xsecs IM.! dsid s)) m

          let mergedHists = M.mapKeysWith (<>) processTitle scaledHists

          let outfname = outfolder args
          createDirectoryIfMissing True outfname
        
          forM_ (M.toList mergedHists) (\(fout, hs) -> runResourceT $ CL.sourceList (fromSGList hs) $$ CL.map (T.encodeUtf8 . showHisto) =$= sinkFile (outfname ++ '/' : (T.unpack fout) ++ ".yoda")) 
