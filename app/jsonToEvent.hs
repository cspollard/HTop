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
main = do
    (s, _) <- sourceHandle stdin =$= ungzip
                $$+ (fileHeader >> sampleInfo >>= yield)
                =$= conduitEncode =$= gzip =$= sinkHandle stdout

    s $$+- tree' =$= conduitEncode =$= gzip =$= sinkHandle stdout

    where
        tree' = do
            comma
            tree :: Conduit BS.ByteString IO Event
            fileFooter
