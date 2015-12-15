{-# LANGUAGE DeriveGeneric #-}

module Data.HEP.Atlas.Muon where

import Data.HEP.LorentzVector

import Data.Binary
import GHC.Generics (Generic)

data Muon = Muon {
    mPtEtaPhiE :: PtEtaPhiE,
    mCharge :: Double,
    mD0Sig :: Double,
    mPtVarCone30 :: Double
    } deriving (Show, Generic)

instance Binary Muon

instance HasLorentzVector Muon where
    lv = fromLV . mPtEtaPhiE

type Muons = [Muon]
