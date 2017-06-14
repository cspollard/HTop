{-# LANGUAGE DeriveGeneric #-}

module BFrag.TrueMuon where

import           Atlas
import           BFrag.PtEtaPhiE
import           Control.Applicative (ZipList (..))
import           Control.Lens
import           Data.Serialize
import           Data.TTree
import           GHC.Float
import           GHC.Generics        (Generic)


data TrueMuon =
  TrueMuon
    { tmPtEtaPhiE :: PtEtaPhiE
    , tmCharge    :: Double
    } deriving (Show, Generic)


instance Serialize TrueMuon where

instance HasLorentzVector TrueMuon where
    toPtEtaPhiE = lens tmPtEtaPhiE $ \m lv -> m { tmPtEtaPhiE = lv }

readTrueMuons :: (MonadIO m, MonadThrow m) => TreeRead m [TrueMuon]
readTrueMuons = do
  tlvs <- lvsFromTTreeF "mu_pt" "mu_eta" "mu_phi" "mu_e"
  chs <- fmap float2Double <$> readBranch "mu_charge"

  let ms = getZipList $ TrueMuon <$> tlvs <*> chs

  return ms
