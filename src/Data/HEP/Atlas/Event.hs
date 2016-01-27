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

import Data.Binary
import GHC.Generics (Generic)

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
    eMET :: PtEtaPhiE
    } deriving (Show, Generic)

instance Binary Event

type Events = [Event]

nJets :: Cut Jet -> Event -> Int
nJets c = length . filter c . eJets

nLargeJets :: Cut LargeJet -> Event -> Int
nLargeJets c = length . filter c . eLargeJets

weight :: Text -> Event -> Double
weight t evt = M.foldr (*) 1 (eEventWeights evt) * (M.!) (eWeightVariations evt) t
