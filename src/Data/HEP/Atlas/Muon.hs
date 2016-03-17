{-# LANGUAGE DeriveGeneric #-}

module Data.HEP.Atlas.Muon where

import Data.HEP.LorentzVector

import Data.Vector (Vector(..))

import Data.Serialize
import GHC.Generics (Generic)

data Muon = Muon {
    mPtEtaPhiE :: PtEtaPhiE,
    mCharge :: Double,
    mD0Sig :: Double,
    mPtVarCone30 :: Double
    } deriving (Show, Generic)

instance Serialize Muon

instance HasLorentzVector Muon where
    lv = fromLV . mPtEtaPhiE

type Muons = Vector Muon
