{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module BFrag.Electron where

import           Atlas
import           BFrag.PtEtaPhiE
import           Control.Applicative (ZipList (..))
import           Control.Lens
import           Data.Serialize
import           Data.TTree
import           GHC.Float
import           GHC.Generics        (Generic)


data Electron =
  Electron
    { ePtEtaPhiE   :: PtEtaPhiE
    , eClEta       :: Double
    , eCharge      :: Int
    , eD0Sig       :: Double
    , ePtVarCone20 :: Double
    } deriving (Show, Generic)

instance Serialize Electron where

instance HasLorentzVector Electron where
    toPtEtaPhiE = lens ePtEtaPhiE $ \e lv -> e { ePtEtaPhiE = lv }

readElectrons :: MonadIO m => TR m [Electron]
readElectrons = do
  tlvs <- lvsFromTTreeF "ElecPt" "ElecEta" "ElecPhi" "ElecE"
  cletas <- fmap float2Double <$> readBranch "ElecClEta"
  chs <- fmap ci2i <$> readBranch "ElecCharge"
  d0sigs <- fmap float2Double <$> readBranch "ElecD0Sig"
  ptvc20s <- fmap float2Double <$> readBranch "ElecMIsol20"
  return . getZipList $ Electron <$> tlvs <*> cletas <*> chs <*> d0sigs <*> ptvc20s

  where
    ci2i :: CInt -> Int
    ci2i = fromEnum

electronHs :: Fills Electron
electronHs = lvHs
