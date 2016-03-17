{-# LANGUAGE DeriveGeneric #-}

module Data.HEP.Atlas.Electron where

import Data.HEP.LorentzVector

import Data.Vector (Vector(..))
 
import Data.Serialize
import GHC.Generics (Generic)

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
