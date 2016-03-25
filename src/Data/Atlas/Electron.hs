{-# LANGUAGE DeriveGeneric #-}

module Data.HEP.Atlas.Electron where


import GHC.Generics (Generic)
import Data.Vector (Vector)
import Data.Serialize
import Data.HEP.LorentzVector
 

data Electron = Electron {
    ePtEtaPhiE :: PtEtaPhiE,
    eClEta :: Double,
    eCharge :: Double,
    eD0Sig :: Double,
    ePtVarCone20 :: Double
    } deriving (Show, Generic)

instance Serialize Electron where

instance HasLorentzVector Electron where
    lv = fromLV . ePtEtaPhiE

type Electrons = Vector Electron
