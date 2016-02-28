{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Parallel.Strategies (using, rseq, parBuffer)

import qualified Data.ByteString.Lazy as BSL

import Data.Binary
import Data.HEP.Atlas.Stream
import Data.HEP.Atlas.Sample
import Data.HEP.Atlas.TopTree

main :: IO ()
main = do
    Sample x y z evts <- parseSample evtWeights evtSystWeights <$> BSL.getContents
    print evts
    -- BSL.putStr . encode . Sample x y z . Stream $ using (unStream evts) (parBuffer 8 rseq)
