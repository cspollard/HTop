{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as BSL

import Data.Maybe (fromJust)

import Data.HEP.Atlas.TopTree
import Data.HEP.Atlas.Event
import Data.HEP.Atlas.Jet
import Data.HEP.Cut
import Data.HEP.LorentzVector

import Data.Histogram.Generic hiding (zip, map, sum)
import qualified Data.Histogram.Generic as H
import Data.Histogram.Fill

import Control.Monad (forM_, liftM)
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Monoid

import qualified Data.Vector as V
import Data.Uncertain

import Data.HEP.Atlas.Histograms 

minPt :: HasLorentzVector a => Double -> Cut a
minPt x = (> x) . lvPt . toPtEtaPhiE

maxAbsEta :: HasLorentzVector a => Double -> Cut a
maxAbsEta x = (< x) . abs . lvEta . toPtEtaPhiE

minMV2c20 :: Double -> Cut Jet
minMV2c20 x = (> x) . jMV2c20

nBtags :: Event -> Int
nBtags = nJets $ minPt 25000 `cAnd` maxAbsEta 2.5 `cAnd` minMV2c20 0.7


main :: IO ()
main = do
        evts <- liftM parseTree BSL.getContents :: IO Events

        let hists = concat $ fillBuilder (eventHists "") evts

        forM_ hists $
            putStr . T.unpack . showHist "/HTop/"
