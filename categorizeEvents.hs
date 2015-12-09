{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as BSL

import Data.HEP.Atlas.Stream
import Data.HEP.Atlas.Event
import Data.HEP.Atlas.Jet
import Data.HEP.Cut
import Data.HEP.LorentzVector

import Data.Histogram hiding (zip)
import Data.Histogram.Fill
import qualified Data.Vector.Unboxed as V

import Data.Aeson

minPt :: HasLorentzVector a => Double -> Cut a
minPt x = (> x) . lvPt . toPtEtaPhiE

maxAbsEta :: HasLorentzVector a => Double -> Cut a
maxAbsEta x = (< x) . abs . lvEta . toPtEtaPhiE

minMV2c20 :: Double -> Cut Jet
minMV2c20 x = (> x) . jMV2c20

nBtags :: Event -> Int
nBtags = nJets $ minPt 25000 `cAnd` maxAbsEta 2.5 `cAnd` minMV2c20 0.7

leadJetPt :: Event -> Double
leadJetPt = lvPt . toPtEtaPhiE . head . eJets


main :: IO ()
main = do

        let hLargeJetPt = forceDouble -<< mkSimple (binD 0 10 5e5) <<- leadJetPt

        evts <- decodeList `fmap` BSL.getContents :: IO Events

        let h = fillBuilder hLargeJetPt $ filter ((==) 1 . nBtags) evts

        print . encode $ V.zip (binsList . bins $ h) (histData h)

        return ()
