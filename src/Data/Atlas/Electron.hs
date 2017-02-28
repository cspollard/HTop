{-# LANGUAGE DeriveGeneric #-}

module Data.Atlas.Electron where

import           Control.Applicative      (ZipList (..))
import           Control.Lens
import           Data.Atlas.Histogramming
import           Data.Atlas.PtEtaPhiE
import           Data.Serialize
import           Data.TTree
import           GHC.Float
import           GHC.Generics             (Generic)


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
  tlvs' <- lvsFromTTreeF "ElecPt" "ElecEta" "ElecPhi" "ElecE"
  let tlvs = over traverse ((lvPt //~ 1e3) . (lvE //~ 1e3)) tlvs'
  cletas <- fmap float2Double <$> readBranch "ElecClEta"
  chs <- fmap ci2i <$> readBranch "ElecCharge"
  d0sigs <- fmap float2Double <$> readBranch "ElecD0Sig"
  ptvc20s <- fmap float2Double <$> readBranch "ElecMIsol20"
  return . getZipList $ Electron <$> tlvs <*> cletas <*> chs <*> d0sigs <*> ptvc20s

  where
    ci2i :: CInt -> Int
    ci2i = fromEnum

electronHs :: Fill Electron
electronHs = lvHs
