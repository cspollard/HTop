{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module BFrag.Muon where

import           Atlas
import           BFrag.PtEtaPhiE
import           Control.Applicative (ZipList (..))
import           Control.Lens
import           Data.Serialize
import           Data.TTree
import           GHC.Float
import           GHC.Generics        (Generic)


data Muon =
  Muon
    { mPtEtaPhiE   :: PtEtaPhiE
    , mCharge      :: Double
    , mD0Sig       :: Double
    , mPtVarCone30 :: Double
    , mPrompt      :: Bool
    } deriving (Show, Generic)

instance Serialize Muon

instance HasLorentzVector Muon where
    toPtEtaPhiE = lens mPtEtaPhiE $ \m lv -> m { mPtEtaPhiE = lv }

readMuons :: (MonadIO m, MonadThrow m) => TreeRead m [Muon]
readMuons = do
  tlvs <- lvsFromTTreeF "mu_pt" "mu_eta" "mu_phi" "mu_e"
  chs <- fmap float2Double <$> readBranch "mu_charge"
  d0sigs <- fmap float2Double <$> readBranch "mu_d0sig"
  ptvc30s <- fmap ((/1e3) . float2Double) <$> readBranch "mu_ptvarcone30"
  prompt <- fmap (== (6 :: CInt)) <$> readBranch "mu_true_type"

  let ms = getZipList $ Muon <$> tlvs <*> chs <*> d0sigs <*> ptvc30s <*> prompt

  return ms
