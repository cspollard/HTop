{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.Text as T

import Data.Conduit
import Data.Conduit.Binary
import Data.Conduit.Zlib (ungzip)
import qualified Data.Conduit.List as CL

import Data.HEP.Atlas.Sample
import Data.HEP.Atlas.TopTree
import Data.HEP.Atlas.Histograms 

import System.IO (stdout, stdin)

import Data.Maybe (fromJust)

import Debug.Trace

main :: IO ()
main = do
        -- TODO
        -- this only works for one file at a time.
        -- cat many files in?
        (s, samp) <- sourceHandle stdin =$= ungzip
                        $$+ (conduitDecode :: Conduit BS.ByteString IO SampleInfo)
                        =$= (fromJust <$> await)

        s $$+- conduitDecode =$= CL.mapM_ (putStr . T.unpack . showHist "/HTop/")
