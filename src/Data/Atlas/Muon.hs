{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Atlas.Muon where

import Control.Lens

import GHC.Generics (Generic)
import Data.Serialize
import Data.Serialize.CTypes ()
import Control.Applicative (ZipList(..))

import Data.HEP.LorentzVector
import Data.Atlas.PtEtaPhiE
import Data.TTree


data Muon = Muon { mPtEtaPhiE :: PtEtaPhiE
                 , mCharge :: CInt
                 , mD0Sig :: Float
                 , mPtVarCone30 :: Float
                 } deriving (Show, Generic)

instance Serialize Muon

instance HasLorentzVector Muon where
    toPtEtaPhiE = lens mPtEtaPhiE $ \m lv -> m { mPtEtaPhiE = lv }

newtype Muons = Muons { fromMuons :: [Muon] } deriving (Show, Generic, Serialize)

instance FromTTree Muons where
    fromTTree = do PtEtaPhiEs tlvs <- lvsFromTTree "MuonPt" "MuonEta" "MuonPhi" "MuonE"
                   chs <- readBranch "MuonCharge"
                   d0sigs <- readBranch "MuonD0Sig"
                   ptvc20s <- readBranch "MuonMIsol20"
                   let ms = Muon <$> ZipList tlvs <*> chs <*> d0sigs <*> ptvc20s
                   return . Muons $ getZipList ms
