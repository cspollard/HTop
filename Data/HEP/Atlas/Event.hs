{-# LANGUAGE DeriveGeneric,OverloadedStrings #-}

module Data.HEP.Atlas.Event where

import Data.HEP.LorentzVector
import Data.HEP.Atlas.Electron
import Data.HEP.Atlas.Muon
import Data.HEP.Atlas.Jet

import Data.Map (Map)
import Data.Text (Text)

import Data.Binary
import GHC.Generics (Generic)

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

nJets :: (Jet -> Bool) -> Event -> Int
nJets cut = length . filter cut . eJets

nLargeJets :: (LargeJet -> Bool) -> Event -> Int
nLargeJets cut = length . filter cut . eLargeJets
