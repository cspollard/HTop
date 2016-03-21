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

import Debug.Trace

sample :: MonadThrow m
          => Consumer ByteString m (SampleInfo, [YodaHisto1D])
sample = do
        s <- sampleInfo' =$= (fromJust <$> await)
        hs <- tree' =$= nominalHistos

        return (s, hs)

    where
        sampleInfo' = fileHeader >> sampleInfo >>= yield

        tree' = do
            comma
            tree :: (MonadThrow m) => Conduit ByteString m Event
            fileFooter


main :: IO ()
main = do
        fns <- getArgs
        samps <- sequence . withStrategy (parBuffer 8 rseq) . map (\fn -> traceShow fn $ runResourceT $ sourceFile fn =$= ungzip $$ sample) $ fns

        hs <- CL.sourceNull $$ nominalHistos
        (s, hs') <- foldM combine (SampleInfo 0 0 0, hs) samps

        let scaledHists = map (`scaleW` (1.0 / sumWeights s)) hs'

        mapM_ (putStr . T.unpack . showHisto) scaledHists

    where

        combine (ss, hs) (ss', hs') = return (ss <> ss', zipWith haddUnsafe hs hs')

