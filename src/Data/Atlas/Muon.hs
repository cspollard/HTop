{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Atlas.Muon where

import           Control.Applicative      (ZipList (..))
import           Control.Lens
import           Data.Atlas.Histogramming
import           Data.Atlas.PtEtaPhiE
import           Data.Atlas.Variation
import           Data.Serialize
import           Data.TTree
import           GHC.Float
import           GHC.Generics             (Generic)


data Muon =
  Muon
    { mPtEtaPhiE   :: PtEtaPhiE
    , mCharge      :: Int
    , mD0Sig       :: Double
    , mPtVarCone30 :: Double
    } deriving (Show, Generic)

instance Serialize Muon

instance HasLorentzVector Muon where
    toPtEtaPhiE = lens mPtEtaPhiE $ \m lv -> m { mPtEtaPhiE = lv }

readMuons :: MonadIO m => TR m [Muon]
readMuons = do
  tlvs <- lvsFromTTreeF "MuonPt" "MuonEta" "MuonPhi" "MuonE"
  chs <- fmap ci2i <$> readBranch "MuonCharge"
  d0sigs <- fmap float2Double <$> readBranch "MuonD0Sig"
  ptvc20s <- fmap float2Double <$> readBranch "MuonMIsol20"
  return . getZipList $ Muon <$> tlvs <*> chs <*> d0sigs <*> ptvc20s

  where
    ci2i :: CInt -> Int
    ci2i = fromEnum

muonHs :: Foldl (Corrected SF Muon) (Vars (Folder YodaObj))
muonHs = lvHs
