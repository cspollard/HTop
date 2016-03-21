{-# LANGUAGE OverloadedStrings, Rank2Types #-}

module Main where

import Data.ByteString (ByteString)
import qualified Data.Text as T

import Data.Conduit
import Data.Conduit.Binary hiding (mapM_)
import Data.Conduit.Zlib (ungzip)
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

import Control.Parallel.Strategies (withStrategy, parBuffer, rseq)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Control.Arrow (second, (&&&))

-- TODO
-- map of dsids to hists
-- group dsids by process

sample :: MonadThrow m
          => Consumer ByteString m (SampleInfo, [YodaHisto1D])
sample = do
        fileHeader
        s <- sampleInfo
        comma
        hs <- tree =$= nominalHistos
        fileFooter

        return (s, fmap (pathPrefix $ T.pack $ '/' : show (dsid s)) hs)


main :: IO ()
main = do
        fns <- getArgs
        samps <- sequence . withStrategy (parBuffer 8 rseq) . map (\fn -> runResourceT $ sourceFile fn =$= ungzip $$ sample) $ fns

        let m = M.fromListWith combine $ map ((dsid . fst) &&& id) samps

        let scaledHists = fmap (\(s, hs) -> fmap (flip scaleW (1.0 / sumWeights s)) hs) m

        mapM_ (mapM_ $ putStr . T.unpack . showHisto) scaledHists

    where
        combine :: (SampleInfo, [YodaHisto1D]) -> (SampleInfo, [YodaHisto1D]) -> (SampleInfo, [YodaHisto1D])
        combine (ss, hs) (ss', hs') = (ss <> ss', zipWith haddUnsafe hs hs')
