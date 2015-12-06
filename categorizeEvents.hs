{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as BSL

import Data.HEP.Atlas.Stream
import Data.HEP.Atlas.Event
import Data.HEP.Atlas.Jet
import Data.HEP.LorentzVector

import Control.Applicative


nBtags :: Event -> Int
nBtags = nJets $
            (&&) <$> ((> 25000) . lvPt . (lv :: Jet -> PtEtaPhiE))
                 <*> ((> 0.7) . jMV2c20)

main :: IO ()
main = do
        evts <- decodeList `fmap` BSL.getContents :: IO Events
        let tag1 = filter ((==) 1 . nBtags) evts :: Events
        let tag2 = filter ((==) 2 . nBtags) evts :: Events

        BSL.writeFile "tag1.bin" $ encodeList tag1

        print "hello!"

        BSL.writeFile "tag2.bin" $ encodeList tag2
