{-# LANGUAGE OverloadedStrings #-}

module Main where

-- TODO
-- parallelism
-- import Control.Parallel.Strategies (using, rseq, parBuffer)

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS

-- import Data.Binary (encode)
-- import Data.HEP.Atlas.TopTree
-- import Data.HEP.Atlas.CrossSections
import System.Environment (getArgs)

import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as B
import Data.HEP.Atlas.Tree
import Data.HEP.Atlas.Event
import Data.HEP.Atlas.TopTree

import System.IO (stdout)

main :: IO ()
main = do
    bs <- fmap BSL.toChunks (BSL.getContents)
    
    (s, _) <- CL.sourceList bs
                $$+ (fileHeader >> sampleInfo >>= yield)
                =$= conduitEncode =$= B.sinkHandle stdout

    s $$+- tree' =$= conduitEncode =$= B.sinkHandle stdout

    where
        tree' = do
            comma
            tree :: Conduit BS.ByteString IO Event
            fileFooter
