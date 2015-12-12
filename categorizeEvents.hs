module Main where

import qualified Data.ByteString.Lazy as BSL

import Data.HEP.Atlas.Stream
import Data.HEP.Atlas.Event
import Data.HEP.Atlas.Jet
import Data.HEP.Cut
import Data.HEP.LorentzVector

import Data.Histogram.Generic hiding (zip)
import Data.Histogram.Fill

import qualified Data.Vector as V

import Data.Hep.Atlas.Histograms 

minPt :: HasLorentzVector a => Double -> Cut a
minPt x = (> x) . lvPt . toPtEtaPhiE

maxAbsEta :: HasLorentzVector a => Double -> Cut a
maxAbsEta x = (< x) . abs . lvEta . toPtEtaPhiE

minMV2c20 :: Double -> Cut Jet
minMV2c20 x = (> x) . jMV2c20

nBtags :: Event -> Int
nBtags = nJets $ minPt 25000 `cAnd` maxAbsEta 2.5 `cAnd` minMV2c20 0.7


toTuple :: IntervalBin b => Histogram V.Vector b a -> V.Vector ((BinValue b, BinValue b), a)
toTuple h = V.zip (binsList . bins $ h) (histData h)

main :: IO ()
main = do
        evts <- decodeList `fmap` BSL.getContents :: IO Events

        mapM_ print . fillBuilder allHists $ evts
