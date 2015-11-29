{-# LANGUAGE DeriveGeneric #-}

module Data.Atlas.Jet where

import Data.HEP.LorentzVector

import Data.Binary
import GHC.Generics (Generic)

data Jet = Jet {
    jPtEtaPhiE :: PtEtaPhiE,
    jMV2c20 :: Double,
    jJVT :: Double
    } deriving (Show, Generic)

instance Binary Jet

instance HasLorentzVector Jet where
    lv = fromLV . jPtEtaPhiE

type Jets = [Jet]

data LargeJet = LargeJet {
    ljPtEtaPhiE :: PtEtaPhiE,
    ljM :: Double,
    ljSD12 :: Double
    } deriving (Show, Generic)

instance Binary LargeJet

instance HasLorentzVector LargeJet where
    lv = fromLV . ljPtEtaPhiE

type LargeJets = [LargeJet]

data TrackJet = TrackJet {
    tjPtEtaPhiE :: PtEtaPhiE,
    tjMV2c20 :: Double
    } deriving (Show, Generic)

instance Binary TrackJet

instance HasLorentzVector TrackJet where
    lv = fromLV . tjPtEtaPhiE

type TrackJets = [TrackJet]
