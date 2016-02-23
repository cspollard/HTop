{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Parallel.Strategies (using, rseq, parBuffer)
import qualified Data.ByteString.Lazy as BSL

import Data.Maybe (fromJust)
import Control.Arrow ((&&&))

import Data.HEP.Atlas.TopTree
import Data.HEP.Atlas.Event
import Data.HEP.Atlas.Jet
import Data.HEP.Cut
import Data.HEP.LorentzVector

import Control.Monad (forM_, liftM)
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Monoid

import qualified Data.Vector as V
import Data.Uncertain

import Data.Histogram
import Data.HEP.Atlas.Histograms
import Data.HEP.Atlas.Stream



main :: IO ()
main = do
        evts <- liftM (parseTree evtWeights evtSystWeights) BSL.getContents :: IO Events

        let hists = concatMap concat $ built $ feed' (eventSystHists ("nominal" : [] {- evtSystWeights -})) $ using evts (parBuffer 8 rseq)
        BSL.putStr . encodeList $ hists


-- example cuts
minPt :: HasLorentzVector a => Double -> Cut a
minPt x = (> x) . lvPt . toPtEtaPhiE

maxAbsEta :: HasLorentzVector a => Double -> Cut a
maxAbsEta x = (< x) . abs . lvEta . toPtEtaPhiE

minMV2c20 :: Double -> Cut Jet
minMV2c20 x = (> x) . jMV2c20

nBtags :: Event -> Int
nBtags = nJets $ minPt 25000 `cAnd` maxAbsEta 2.5 `cAnd` minMV2c20 0.7
