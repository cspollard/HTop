{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Data.Atlas.Event where

import Data.HEP.LorentzVector
import Data.Atlas.Electron
import Data.Atlas.Muon
import Data.Atlas.Jet

import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)

import Data.Serialize
import GHC.Generics (Generic)

import Data.Serialize.Text ()

data Event = Event {
    eRunNumber :: Int,
    eEventNumber :: Int,
    eMu :: Double,
    eElectrons :: Electrons,
    eMuons :: Muons,
    eJets :: Jets,
    eLargeJets :: LargeJets,
    eTrackJets :: TrackJets,
    eMET :: PtEtaPhiE
    } deriving (Show, Generic)

instance Serialize Event where

type Events = [Event]

-- TODO
-- How do we want to deal with syst weights?
type EventWeights = Map Text Double

weight :: EventWeights -> [Text] -> Double
weight ew ts = product $ map (ew M.!) ts
