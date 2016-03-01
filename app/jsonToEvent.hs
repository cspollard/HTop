{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Parallel.Strategies (using, rseq, parBuffer)

import qualified Data.ByteString.Lazy as BSL
import Data.Attoparsec.ByteString.Lazy

import Data.Binary (encode)
import Data.HEP.Atlas.TopTree
import Data.HEP.Atlas.CrossSections
import Data.Maybe (fromJust)
import System.Environment (getArgs)

main :: IO ()
main = do
    csname <- head <$> getArgs
    cs <- (fromJust . maybeResult . parse crossSectionInfo) <$> BSL.readFile csname
    samp <- parseTopSample evtWeights evtSystWeights cs <$> BSL.getContents
    BSL.putStr . encode $ samp
