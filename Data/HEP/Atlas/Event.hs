{-# LANGUAGE DeriveGeneric,OverloadedStrings #-}

module Data.HEP.Atlas.Event where

import Data.HEP.LorentzVector
import Data.HEP.Cut
import Data.HEP.Atlas.Electron
import Data.HEP.Atlas.Muon
import Data.HEP.Atlas.Jet

import Data.Map (Map)
import Data.Text (Text)

import Data.Binary
import GHC.Generics (Generic)

import Control.Applicative

data Event = Event {
    eRunNumber :: Int,
    eEventNumber :: Int,
    eMCChannelNumber :: Int,
    eEventWeights :: Map Text Double,
    eWeightVariations :: Map Text Double,
    eMu :: Double,
    eElectrons :: Electrons,
    eMuons :: Muons,
    eJets :: Jets,
    eLargeJets :: LargeJets,
    eMET :: PtEtaPhiE
    } deriving (Show, Generic)

instance Binary Event

type Events = [Event]


nBtag :: Cut Event
nBtag = CInt "nBtag" $ length . filter ((> 0.4) <$> jMV2c20) . eJets

nJets :: Cut Event
nJets = CInt "nJets" $ length . filter ((> 25000) <$> (lvPt . jPtEtaPhiE)) . eJets

nLargeJets :: Cut Event
nLargeJets = CInt "nLargeJets" $ length . filter ((> 200000) <$> (lvPt . ljPtEtaPhiE)) . eLargeJets
