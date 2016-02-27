{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Parallel.Strategies (using, rseq, parBuffer)

import qualified Data.ByteString.Lazy as BSL

import Data.HEP.Atlas.Stream
import Data.HEP.Atlas.TopTree

main :: IO ()
main = do
    evts <- parseTree evtWeights evtSystWeights <$> BSL.getContents
    BSL.putStr . encodeList $ using evts (parBuffer 8 rseq)
