{-# LANGUAGE DeriveGeneric #-}

module Data.Atlas.Jet where

import Data.HEP.LorentzVector

import Data.Serialize
import GHC.Generics (Generic)

import Data.Atlas.LorentzVector
import Data.TTree


data Jet = Jet { jPtEtaPhiE :: PtEtaPhiE
               , jMV2c10 :: Float
               , jJVT :: Float
               } deriving (Show, Generic)

instance Serialize Jet

instance HasLorentzVector Jet where
    lv = fromLV . jPtEtaPhiE

newtype Jets = Jets [Jet]

instance FromTTree Jets where
    fromTTree = Jets . fromZipList
                <$> lvsFromTTree "jet"
                <*> readBranch "jet_mv2c10"
                <*> readBranch "jet_jvt"


data LargeJet = LargeJet { ljPtEtaPhiE :: PtEtaPhiE
                         , ljM :: Float
                         , ljSD12 :: Float
                         , ljTau21 :: Float
                         , ljTau32 :: Float
                         -- TODO
                         -- can't handle vec<vec<int>> yet
                         -- , ljGhostTJs :: [Int]
                         } deriving (Show, Generic)


instance Serialize LargeJet

instance HasLorentzVector LargeJet where
    lv = fromLV . ljPtEtaPhiE

newtype LargeJets = LargeJets [LargeJet]

instance FromTTree LargeJets where
    fromTTree = LargeJets . fromZipList
                <$> lvsFromTTree "ljet"
                <*> readBranch "ljet_sd12"
                <*> readBranch "ljet_tau21"
                <*> readBranch "ljet_tau32"


data TrackJet = TrackJet {
    tjPtEtaPhiE :: PtEtaPhiE,
    tjMV2c10 :: Float,
    -- TODO
    -- this won't be present in data.
    -- tjLabel :: Maybe Int
    } deriving (Show, Generic)


instance Serialize TrackJet

instance HasLorentzVector TrackJet where
    lv = fromLV . tjPtEtaPhiE

newtype TrackJets = TrackJets [TrackJet]

instance FromTTree TrackJets where
    fromTTree = TrackJets . fromZipList
                <$> lvsFromTTree "tjet"
                <*> readBranch "tjet_mv2c10"
