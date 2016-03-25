{-# LANGUAGE DeriveGeneric #-}

module Data.Atlas.Muon where

import GHC.Generics (Generic)
import Data.Vector (Vector)
import Data.Serialize
import Data.HEP.LorentzVector

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
