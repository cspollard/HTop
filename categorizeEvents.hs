{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as BSL

import Data.HEP.Atlas.Stream
import Data.HEP.Atlas.Event
import Data.HEP.Atlas.Jet
import Data.HEP.Cut
import Data.HEP.LorentzVector
import Data.Uncertain

import Data.Histogram.Generic hiding (zip)
import Data.Histogram.Fill

import qualified Data.Vector.Generic as VG
import qualified Data.Vector as V

import Data.Traversable (sequenceA)

import Data.Aeson

import Control.Arrow

minPt :: HasLorentzVector a => Double -> Cut a
minPt x = (> x) . lvPt . toPtEtaPhiE

maxAbsEta :: HasLorentzVector a => Double -> Cut a
maxAbsEta x = (< x) . abs . lvEta . toPtEtaPhiE

minMV2c20 :: Double -> Cut Jet
minMV2c20 x = (> x) . jMV2c20

nBtags :: Event -> Int
nBtags = nJets $ minPt 25000 `cAnd` maxAbsEta 2.5 `cAnd` minMV2c20 0.7

ptHist :: (HasLorentzVector a, Num val) => Int -> Double -> Double -> HBuilder (a, val) (Histogram V.Vector BinD val)
ptHist n xmin xmax = mkWeightedG (binD xmin n xmax) <<- f
        where f = first (lvPt . toPtEtaPhiE)


leadJet :: Event -> Jet
leadJet = head . eJets


main :: IO ()
main = do

        let hLeadJetPt = ptHist 10 0 5e5 <<- (leadJet &&& weight)

        evts <- decodeList `fmap` BSL.getContents :: IO Events

        let h = fillBuilder hLeadJetPt $ filter ((==) 1 . nBtags) evts

        print $ V.zip (binsList . bins $ h) (histData h)

        return ()
