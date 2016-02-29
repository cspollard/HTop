{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Parallel.Strategies (using, rseq, parBuffer)

import qualified Data.ByteString.Lazy as BSL

import Data.HEP.Atlas.Stream
import Data.HEP.Atlas.TopTree
import Data.Maybe (fromJust)

main :: IO ()
main = do
    evts <- parseTopSample evtWeights evtSystWeights <$> BSL.getContents
    BSL.putStr . encodeList . map fromJust $ using evts (parBuffer 8 rseq)
