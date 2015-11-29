{-# LANGUAGE DeriveGeneric #-}

module Data.Atlas.Event where

import Data.HEP.LorentzVector
import Data.Atlas.Electron
import Data.Atlas.Muon
import Data.Atlas.Jet

import Data.Map
import Data.Text

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
