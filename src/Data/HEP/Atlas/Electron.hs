{-# LANGUAGE DeriveGeneric #-}

module Data.HEP.Atlas.Electron where

import Data.HEP.LorentzVector
 
import Data.Binary
import GHC.Generics (Generic)

data Electron = Electron {
    ePtEtaPhiE :: PtEtaPhiE,
    eClEta :: Double,
    eCharge :: Double,
    eD0Sig :: Double,
    ePtVarCone20 :: Double
    } deriving (Show, Generic)

instance Binary Electron

instance HasLorentzVector Electron where
    lv = fromLV . ePtEtaPhiE

type Electrons = [Electron]
