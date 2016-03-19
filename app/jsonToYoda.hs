{-# LANGUAGE OverloadedStrings, Rank2Types #-}

module Main where

import Data.ByteString (ByteString)
import qualified Data.Text as T

import Data.Conduit
import Data.Conduit.Binary hiding (mapM_)
import Data.Conduit.Zlib (gzip, ungzip)
import qualified Data.Conduit.List as CL

import Control.Monad (foldM, mapM_)
import Control.Monad.Trans.Resource (MonadResource, runResourceT)
import Control.Monad.Catch (MonadThrow)

import System.IO (stdout, stdin)

import Data.Maybe (fromJust)
import Data.Monoid
import System.Environment (getArgs)

import Data.Histogram
import Data.Builder

import Data.HEP.Atlas.Tree
import Data.HEP.Atlas.Sample
import Data.HEP.Atlas.Event
import Data.HEP.Atlas.TopTree
import Data.HEP.Atlas.Histograms



addSample :: MonadThrow m
          => (SampleInfo, Builder Event h)
          -> Consumer ByteString m (SampleInfo, Builder Event h)
addSample (s, b) = do
        s' <- sampleInfo' =$= (fromJust <$> await)
        b' <- tree' =$= CL.fold build b

        return (s <> s', b')

    where
        sampleInfo' = fileHeader >> sampleInfo >>= yield

        tree' = do
            comma
            tree :: (MonadThrow m) => Conduit ByteString m Event
            fileFooter


main :: IO ()
main = do
        (s, b) <- foldM (\s fn -> runResourceT $ sourceFile fn =$= ungzip $$ addSample s) def
                        =<< getArgs


        let b' = built b :: [(T.Text, [[[YodaHisto1D]]])]

        -- built b :: [(T.Text, [[[YodaHisto1D]]])]
        let hists = fmap (concatMap concat) <$> built b :: [(T.Text, [YodaHisto1D])]

        let scaledHists = fmap (map (alterHisto (fmap (`scaleW` (1.0 / sumWeights s))))) <$> hists

        mapM_ (\(n, hs) -> mapM_ (putStr . T.unpack . showHisto ("/HTop/" <> n)) hs) scaledHists


    where
        def = (SampleInfo 0 0 0, channelSystHistos ["nominal"])

