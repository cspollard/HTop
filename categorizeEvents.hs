{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as BSL

import Data.HEP.Atlas.Stream
import Data.HEP.Atlas.Event
import Data.HEP.Atlas.Jet
import Data.HEP.Cut
import Data.HEP.LorentzVector

import Control.Applicative



minPt :: HasLorentzVector a => Double -> Cut a
minPt x = Cut $ (> x) . lvPt . toPtEtaPhiE

maxAbsEta :: HasLorentzVector a => Double -> Cut a
maxAbsEta x = Cut $ (< x) . abs . lvEta . toPtEtaPhiE

minMV2c20 :: Double -> Cut Jet
minMV2c20 x = Cut $ (> x) . jMV2c20

nBtags :: Event -> Int
nBtags = nJets . cut $ cAnd (minPt 25000) (minMV2c20 0.7)

main :: IO ()
main = do
        evts <- decodeList `fmap` BSL.getContents :: IO Events
        let tag1 = filter ((==) 1 . nBtags) evts :: Events
        let tag2 = filter ((==) 2 . nBtags) evts :: Events

        print "hello!"

        BSL.writeFile "tag2.bin" $ encodeList tag2

        return ()
