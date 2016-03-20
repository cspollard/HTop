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

-- import System.IO (stdout, stdin)

import Data.Maybe (fromJust)
import Data.Monoid
import System.Environment (getArgs)

import Data.Histogram

import Data.HEP.Atlas.Tree
import Data.HEP.Atlas.Sample
import Data.HEP.Atlas.Event
import Data.HEP.Atlas.TopTree
import Data.HEP.Atlas.Histograms

import Control.Parallel.Strategies (rpar, withStrategy, parList)


sample :: MonadThrow m
          => Consumer ByteString m (SampleInfo, [(T.Text, [YodaHisto1D])])
sample = do
        s <- sampleInfo' =$= (fromJust <$> await)
        hs <- fmap built $ tree' =$= CL.fold build (channelSystHistos ["nominal"])

        return (s, hs)

    where
        sampleInfo' = fileHeader >> sampleInfo >>= yield

        tree' = do
            comma
            tree :: (MonadThrow m) => Conduit ByteString m Event
            fileFooter


main :: IO ()
main = do
        (s, b) <- foldM combine def
               =<< {- (withStrategy (parList rpar) <$> -}
               mapM (\fn -> runResourceT $ (sourceFile fn =$= ungzip $$ sample))
               =<< getArgs


        let scaledHists = fmap (map (alterHisto (fmap (`scaleW` (1.0 / sumWeights s))))) <$> b

        mapM_ (\(n, hs) -> mapM_ (putStr . T.unpack . showHisto ("/HTop/" <> n)) hs) scaledHists

    where
        def = (SampleInfo 0 0 0, built $ channelSystHistos ["nominal"])

        combine :: (SampleInfo, [(T.Text, [YodaHisto1D])])
                -> (SampleInfo, [(T.Text, [YodaHisto1D])])
                -> IO (SampleInfo, [(T.Text, [YodaHisto1D])])
        combine (ss, hs) (ss', hs') = return (ss <> ss', zipWith combineChan hs hs')
        combineChan (t, hs) (_, hs') = (t, zipWith haddUnsafe hs hs')

