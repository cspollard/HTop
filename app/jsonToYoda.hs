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

import Data.HEP.Atlas.Tree
import Data.HEP.Atlas.Sample
import Data.HEP.Atlas.TopTree
import Data.HEP.Atlas.Histograms
import Data.HEP.Atlas.CrossSections
import Data.HEP.Atlas.ProcessInfo

import Control.Parallel.Strategies (withStrategy, parBuffer, rseq)

import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM

import Control.Arrow ((&&&))

import Options.Generic
import Data.Aeson (FromJSON)
import Data.SGList

import Debug.Trace

-- TODO
-- at some point this all needs to be moved into library functions.
-- only basic conduits (and args?) should be left here.

data Args = Args { outfolder :: String
                 , infiles :: String
                 , xsecfile :: String
                 } deriving (Show, Generic)

instance ParseRecord Args where


main :: IO ()
main = do args <- getRecord "jsonToYoda" :: IO Args

          fins <- runResourceT $ sourceFile (infiles args) =$= CB.lines =$= CL.map (T.unpack . T.decodeUtf8) $$ CL.consume
          xsecs <- runResourceT $ sourceFile (xsecfile args) $$ sinkParser crossSectionInfo

          samps <- sequence . withStrategy (parBuffer 1 rseq) . map (\fn -> traceShow fn $ runResourceT $ sourceFile fn =$= ungzip $$ project nominalHistos) $ fins

          let m = M.fromListWith (<>) $ map ((dsid . fst) &&& id) (samps :: [Sample (SGList YodaHisto1D)])

          let scaledHists = fmap (\ss@(s, _) -> freezeSample ss `scaleW` (xsecs IM.! dsid s)) m

          let mergedHists = M.mapKeysWith (<>) processTitle scaledHists

          let outfname = outfolder args
          createDirectoryIfMissing True outfname
        
          forM_ (M.toList mergedHists) (\(fout, hs) -> runResourceT $ CL.sourceList (fromSGList hs) $$ CL.map (T.encodeUtf8 . showHisto) =$= sinkFile (outfname ++ '/' : (T.unpack fout) ++ ".yoda")) 
