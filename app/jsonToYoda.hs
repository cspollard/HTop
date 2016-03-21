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

import Control.Monad (foldM)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Catch (MonadThrow)

import Data.Maybe (fromJust)
import Data.Monoid
import System.Environment (getArgs)

import Data.Histogram

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


-- TODO
-- at some point this all needs to be moved into library functions.
-- only basic conduits (and args?) should be left here.

data Args = Args { outfile :: String
                 , infiles :: String
                 , xsecfile :: String
                 } deriving (Show, Generic)

instance ParseRecord Args where


-- TODO
-- group dsids by process

sample :: MonadThrow m
          => Consumer ByteString m (SampleInfo, [YodaHisto1D])
sample = do fileHeader
            s <- sampleInfo
            comma
            hs <- tree =$= nominalHistos
            fileFooter

            return (s, hs)


main :: IO ()
main = do args <- getRecord "jsonToYoda" :: IO Args
          let fout = outfile args
          fins <- runResourceT $ sourceFile (infiles args) =$= CB.lines =$= CL.map (T.unpack . T.decodeUtf8) $$ CL.consume
          xsecs <- runResourceT $ sourceFile (xsecfile args) $$ sinkParser crossSectionInfo

          samps <- sequence . withStrategy (parBuffer 1 rseq) . map (\fn -> runResourceT $ sourceFile fn =$= ungzip $$ sample) $ fins

          let m = M.fromListWith combine $ map ((dsid . fst) &&& id) samps

          let scaledHists = fmap (\(s, hs) -> fmap (flip scaleW $ (xsecs IM.! dsid s) * (1.0 / sumWeights s)) hs) m

          let mergedHists = M.mapWithKey (\s -> fmap (pathPrefix $ T.cons '/' s)) $ M.mapKeysWith (zipWith haddUnsafe) processTitle scaledHists

          runResourceT $ yield mergedHists =$= CL.concat =$= CL.mapFoldable (fmap showHisto) $$ CL.map T.encodeUtf8 =$= sinkFile fout

    where
        combine :: (SampleInfo, [YodaHisto1D]) -> (SampleInfo, [YodaHisto1D]) -> (SampleInfo, [YodaHisto1D])
        combine (ss, hs) (ss', hs') = (ss <> ss', zipWith haddUnsafe hs hs')
