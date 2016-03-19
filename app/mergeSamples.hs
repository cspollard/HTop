{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.Text as T

import Data.Conduit
import Data.Conduit.Binary
import Data.Conduit.Zlib (gzip, ungzip)
import qualified Data.Conduit.List as CL

import Data.HEP.Atlas.Sample
import Data.HEP.Atlas.TopTree
import Data.HEP.Atlas.Histograms 

import System.IO (stdout, stdin)

import Data.Maybe (fromJust)

import Data.Serialize
import Data.Histogram
import Data.Builder

import Control.Monad (mapM)
import Control.Monad.Catch
import Data.Monoid ((<>))

import Data.List (foldl1)


import Data.List (foldl')
import System.Environment (getArgs)

import Control.Monad.Trans.Resource (MonadResource, runResourceT)

decodeSample :: (MonadThrow m, Serialize h)
                => Consumer BS.ByteString m (SampleInfo, [h])
decodeSample = do samp <- conduitDecode =$= (fromJust <$> await)
                  hists <- conduitDecode =$= CL.consume
                  return (samp, hists)

main :: IO ()
main = do 
        samps <- mapM (\fn -> runResourceT $ sourceFile fn =$= ungzip $$ decodeSample) =<< getArgs

        -- TODO
        -- this can cause an error
        let (samp, hists) = foldl1 addSamp samps

        ((yield samp =$= conduitEncode) >> (CL.sourceList hists =$= conduitEncode))
            $$ gzip =$= sinkHandle stdout

    where
        addSamp :: (SampleInfo, [YodaHisto1D]) -> (SampleInfo, [YodaHisto1D]) -> (SampleInfo, [YodaHisto1D])
        addSamp (ss, hs) (ss', hs') = (ss <> ss', zipWith haddUnsafe hs hs')
