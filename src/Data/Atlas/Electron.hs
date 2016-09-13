{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Atlas.Electron where

import Control.Lens

import GHC.Generics (Generic)
import Data.Serialize
import Data.Serialize.CTypes ()
import Control.Applicative (ZipList(..))

import Data.HEP.LorentzVector
import Data.Atlas.PtEtaPhiE
import Data.TTree
 

data Electron = Electron { ePtEtaPhiE :: PtEtaPhiE
                         , eClEta :: Float
                         , eCharge :: CInt
                         , eD0Sig :: Float
                         , ePtVarCone20 :: Float
                         } deriving (Show, Generic)

instance Serialize Electron where

instance HasLorentzVector Electron where
    toPtEtaPhiE = lens ePtEtaPhiE $ \e lv -> e { ePtEtaPhiE = lv }

newtype Electrons = Electrons { fromElectrons :: [Electron] } deriving (Show, Generic, Serialize)

instance FromTTree Electrons where
    fromTTree = do PtEtaPhiEs tlvs <- lvsFromTTree "ElecPt" "ElecEta" "ElecPhi" "ElecE"
                   cletas <- readBranch "ElecClEta"
                   chs <- readBranch "ElecCharge"
                   d0sigs <- readBranch "ElecD0Sig"
                   ptvc20s <- readBranch "ElecMIsol20"
                   let es = Electron <$> ZipList tlvs <*> cletas <*> chs <*> d0sigs <*> ptvc20s
                   return . Electrons $ getZipList es
