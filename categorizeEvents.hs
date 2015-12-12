{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Main where

import qualified Data.ByteString.Lazy as BSL

import Data.Maybe (isJust, fromJust, listToMaybe)

import Data.HEP.Atlas.Stream
import Data.HEP.Atlas.Event
import Data.HEP.Atlas.Jet
import Data.HEP.Cut
import Data.HEP.LorentzVector
import Data.Uncertain

import Data.Text (Text)

import Data.Histogram.Generic hiding (zip)
import Data.Histogram.Fill

import qualified Data.Vector as V

import Data.Traversable (sequenceA)
import Control.Applicative ((<$>))
import Control.Arrow

minPt :: HasLorentzVector a => Double -> Cut a
minPt x = (> x) . lvPt . toPtEtaPhiE

maxAbsEta :: HasLorentzVector a => Double -> Cut a
maxAbsEta x = (< x) . abs . lvEta . toPtEtaPhiE

minMV2c20 :: Double -> Cut Jet
minMV2c20 x = (> x) . jMV2c20

nBtags :: Event -> Int
nBtags = nJets $ minPt 25000 `cAnd` maxAbsEta 2.5 `cAnd` minMV2c20 0.7

ptBins :: BinD
ptBins = binD 0 50 500e3

eBins :: BinD
eBins = binD 0 50 500e3

mBins :: BinD
mBins = binD 0 50 200e3

etaBins :: BinD
etaBins = binD (-3) 50 3

phiBins :: BinD
phiBins = binD (-pi) 50 pi


type Named a = (Text, a)

-- clean both of these up
objHists :: (HasLorentzVector a, Num val) => HBuilder (a, val) [Named (Histogram V.Vector BinD val)]
objHists = sequenceA [
                      ("pT", ) <$> mkWeightedG ptBins <<- first lvPt,
                      ("E", ) <$> mkWeightedG eBins <<- first lvE,
                      ("m", ) <$> mkWeightedG mBins <<- first lvM,
                      ("eta", ) <$> mkWeightedG etaBins <<- first lvEta,
                      ("phi", ) <$> mkWeightedG phiBins <<- first lvPhi
                     ] <<- first toPtEtaPhiE

-- make leadHist, secondHist, etc
eventHists :: HBuilder Event [Named ([Named (Histogram V.Vector BinD (U Double))])]
eventHists = sequenceA [
                         ("Jets", ) <$> objHists <<- first fromJust <<? (isJust . fst) <<- (first (listToMaybe . eJets)),
                         ("LargeJets", ) <$> objHists <<- first fromJust <<? (isJust . fst) <<- (first (listToMaybe . eLargeJets)),
                         ("Muons", ) <$> objHists <<- first fromJust <<? (isJust . fst) <<- (first (listToMaybe . eMuons)),
                         ("Electrons", ) <$> objHists <<- first fromJust <<? (isJust . fst) <<- (first (listToMaybe . eElectrons)),
                         ("MET", ) <$> objHists <<- first eMET
                        ] <<- (id &&& weight)


toTuple :: IntervalBin b => Histogram V.Vector b a -> V.Vector ((BinValue b, BinValue b), a)
toTuple h = V.zip (binsList . bins $ h) (histData h)

main :: IO ()
main = do
        evts <- decodeList `fmap` BSL.getContents :: IO Events

        mapM_ print . fillBuilder eventHists $ evts
