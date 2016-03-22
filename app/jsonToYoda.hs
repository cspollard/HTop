{-# LANGUAGE OverloadedStrings, Rank2Types, DeriveGeneric #-}

module Main where

import Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Data.Conduit
import Data.Conduit.Binary hiding (mapM_)
import qualified Data.Conduit.Binary as CB

import Data.Conduit.Zlib (ungzip)
import Data.Conduit.Attoparsec
import qualified Data.Conduit.List as CL

import Control.Monad (foldM, forM_)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Catch (MonadThrow)

import Data.Maybe (fromJust)
import Data.Semigroup
import System.Environment (getArgs)

import Data.Histogram

import System.Directory

import Data.HEP.Atlas.Tree
import Data.HEP.Atlas.Sample
import Data.HEP.Atlas.Event
import Data.HEP.Atlas.TopTree
import Data.HEP.Atlas.Histograms
import Data.HEP.Atlas.CrossSections
import Data.HEP.Atlas.ProcessInfo

import Control.Parallel.Strategies (withStrategy, parBuffer, rseq)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM

import Control.Arrow ((&&&))

import Options.Generic
import GHC.Generics

import Data.Aeson (FromJSON)

import Debug.Trace

-- TODO
-- at some point this all needs to be moved into library functions.
-- only basic conduits (and args?) should be left here.

data Args = Args { outfolder :: String
                 , infiles :: String
                 , xsecfile :: String
                 } deriving (Show, Generic)

instance ParseRecord Args where


project :: (FromJSON a, MonadThrow m)
          => Consumer a m h
          -> Consumer ByteString m (Sample h)
project c = do fileHeader
               s <- sampleInfo
               comma
               h <- tree =$= c
               fileFooter

               return (s, h)


main :: IO ()
main = do args <- getRecord "jsonToYoda" :: IO Args

          fins <- runResourceT $ sourceFile (infiles args) =$= CB.lines =$= CL.map (T.unpack . T.decodeUtf8) $$ CL.consume
          xsecs <- runResourceT $ sourceFile (xsecfile args) $$ sinkParser crossSectionInfo

          samps <- sequence . withStrategy (parBuffer 1 rseq) . map (\fn -> traceShow fn $ runResourceT $ sourceFile fn =$= ungzip $$ project nominalHistos) $ fins

          let m = M.fromListWith (<>) $ map ((dsid . fst) &&& id) (samps :: [Sample (SGList YodaHisto1D)])

          let scaledHists = fmap (\ss@(s, _) -> freezeSample ss (xsecs IM.! dsid s)) m

          let mergedHists = M.mapKeysWith (<>) processTitle scaledHists

          let outfname = outfolder args
          createDirectoryIfMissing True outfname
        
          forM_ (M.toList mergedHists) (\(fout, hs) -> runResourceT $ CL.sourceList (fromSGList hs) $$ CL.map (T.encodeUtf8 . showHisto) =$= sinkFile (outfname ++ '/' : (T.unpack fout) ++ ".yoda")) 

    where
        combine :: (SampleInfo, [YodaHisto1D]) -> (SampleInfo, [YodaHisto1D]) -> (SampleInfo, [YodaHisto1D])
        combine (ss, hs) (ss', hs') = (ss <> ss', zipWith haddUnsafe hs hs')
