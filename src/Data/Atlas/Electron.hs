{-# LANGUAGE DeriveGeneric #-}

module Data.Atlas.Electron where

import GHC.Generics (Generic)
import Data.Vector (Vector)
import Data.Serialize
import Data.HEP.LorentzVector
import Data.Atlas.LorentzVector

import Data.TTree
 

data Electron = Electron { ePtEtaPhiE :: PtEtaPhiE
                         , eClEta :: Float
                         , eCharge :: Float
                         , eD0Sig :: Float
                         , ePtVarCone20 :: Float
                         } deriving (Show, Generic)

instance Serialize Electron where

instance HasLorentzVector Electron where
    lv = fromLV . ePtEtaPhiE

newtype Electrons = Electrons [Electron]

instance FromTTree Electrons where
    fromTTree = Electrons . fromZipList
                <$> lvsFromTTree "el"
                <*> readBranch "el_cl_eta"
                <*> readBranch "el_charge"
                <*> readBranch "el_d0sig"
                <*> readBranch "el_ptvarcone20"
