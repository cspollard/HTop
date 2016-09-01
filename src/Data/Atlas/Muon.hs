{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Atlas.Muon where

import GHC.Generics (Generic)
import Data.Serialize
import Control.Applicative (ZipList(..))

import Data.HEP.LorentzVector
import Data.Atlas.PtEtaPhiE
import Data.TTree


data Muon = Muon {
    mPtEtaPhiE :: PtEtaPhiE,
    mCharge :: Double,
    mD0Sig :: Double,
    mPtVarCone30 :: Double
    } deriving (Show, Generic)

instance Serialize Muon

instance HasLorentzVector Muon where
    lv = fromLV . mPtEtaPhiE

newtype Muons = Muons { fromMuons :: [Muon] } deriving (Show, Generic, Serialize)

instance FromTTree Muons where
    fromTTree = do PtEtaPhiEs tlvs <- lvsFromTTree "MuonPt" "MuonEta" "MuonPhi" "MuonE"
                   chs <- readBranch "MuonCharge"
                   d0sigs <- readBranch "MuonD0Sig"
                   ptvc20s <- readBranch "MuonMIsol20"
                   let ms = Muon <$> ZipList tlvs <*> chs <*> d0sigs <*> ptvc20s
                   return . Muons $ getZipList ms
