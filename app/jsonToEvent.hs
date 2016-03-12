{-# LANGUAGE OverloadedStrings #-}

module Main where

-- TODO
-- parallelism
-- import Control.Parallel.Strategies (using, rseq, parBuffer)

import qualified Data.ByteString.Lazy as BSL

-- import Data.Binary (encode)
-- import Data.HEP.Atlas.TopTree
-- import Data.HEP.Atlas.CrossSections
import System.Environment (getArgs)

import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.HEP.Atlas.Tree
import Data.HEP.Atlas.Event
import Data.HEP.Atlas.TopTree


main :: IO ()
main = do
    -- csname <- head <$> getArgs
    bs <- fmap BSL.toChunks (BSL.getContents)
    
    (s, samp) <- CL.sourceList bs $$+ (fileHeader >> sampleInfo)
    print samp

    s $$+- tree' =$= CL.mapM_ (print :: Event -> IO ())

    where
        tree' = do
            comma
            tree
            fileFooter
