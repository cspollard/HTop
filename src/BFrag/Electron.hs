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
    , eCharge      :: Double
    , eD0Sig       :: Double
    , ePtVarCone20 :: Double
    } deriving (Show, Generic)

instance Serialize Electron where

instance HasLorentzVector Electron where
    toPtEtaPhiE = lens ePtEtaPhiE $ \e lv -> e { ePtEtaPhiE = lv }

readElectrons :: (MonadIO m, MonadThrow m) => TreeRead m [Electron]
readElectrons = do
  tlvs <- lvsFromTTreeF "el_pt" "el_eta" "el_phi" "el_e"
  cletas <- fmap float2Double <$> readBranch "el_cl_eta"
  chs <- fmap float2Double <$> readBranch "el_charge"
  d0sigs <- fmap float2Double <$> readBranch "el_d0sig"
  ptvc20s <- fmap ((/1e3) . float2Double) <$> readBranch "el_ptvarcone20"

  let es =
        getZipList
        $ Electron <$> tlvs <*> cletas <*> chs <*> d0sigs <*> ptvc20s

  return es

electronHs :: Foldl (Electron, Double) YodaFolder
electronHs = lvHs
