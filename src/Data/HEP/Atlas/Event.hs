{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Data.HEP.Atlas.Event where

import Data.HEP.LorentzVector
import Data.HEP.Cut
import Data.HEP.Atlas.Electron
import Data.HEP.Atlas.Muon
import Data.HEP.Atlas.Jet

import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)

import qualified Data.Vector as V

import Data.Serialize
import GHC.Generics (Generic)

import Data.Serialize.Text ()

-- TODO
-- How do we want to deal with syst weights?
data Event = Event {
    eRunNumber :: Int,
    eEventNumber :: Int,
    eMCChannelNumber :: Maybe Int,
    eEventWeights :: Map Text Double,
    eWeightVariations :: Map Text Double,
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

weight :: Text -> Event -> Double
weight t evt = M.foldr (*) 1 (eEventWeights evt) * (M.!) (eWeightVariations evt) t
