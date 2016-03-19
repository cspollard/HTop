{-# LANGUAGE OverloadedStrings #-}

module Main where

-- TODO
-- parallelism
-- import Control.Parallel.Strategies (using, rseq, parBuffer)

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS

import Data.Conduit
import Data.Conduit.Binary
import Data.Conduit.Zlib (gzip, ungzip)

import Data.HEP.Atlas.Tree
import Data.HEP.Atlas.Event
import Data.HEP.Atlas.TopTree

import System.IO (stdout, stdin)


main :: IO ()
main = sourceHandle stdin =$= ungzip
            =$= (sampleInfo' >> tree')
            $$  gzip =$= sinkHandle stdout

    where
        sampleInfo' = (fileHeader >> sampleInfo >>= yield) =$= conduitEncode

        tree' = do
            comma
            (tree :: Conduit BS.ByteString IO Event) =$= conduitEncode
            fileFooter
