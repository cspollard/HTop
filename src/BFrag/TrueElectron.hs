{-# LANGUAGE DeriveGeneric #-}

module BFrag.TrueElectron where

import           Atlas
import           BFrag.PtEtaPhiE
import           Control.Applicative (ZipList (..))
import           Control.Lens
import           Data.Serialize
import           Data.TTree
import           GHC.Float
import           GHC.Generics        (Generic)


data TrueElectron =
  TrueElectron
    { tePtEtaPhiE :: PtEtaPhiE
    , teCharge    :: Double
    } deriving (Show, Generic)


instance Serialize TrueElectron where

instance HasLorentzVector TrueElectron where
    toPtEtaPhiE = lens tePtEtaPhiE $ \e lv -> e { tePtEtaPhiE = lv }

readTrueElectrons :: (MonadIO m, MonadThrow m) => TreeRead m [TrueElectron]
readTrueElectrons = do
  tlvs <- lvsFromTTreeF "el_pt" "el_eta" "el_phi" "el_e"
  chs <- fmap float2Double <$> readBranch "el_charge"

  let es = getZipList $ TrueElectron <$> tlvs <*> chs

  return es
