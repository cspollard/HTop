{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Atlas.Muon where

import           Control.Applicative      (ZipList (..))
import           Control.Lens
import           Data.Atlas.Histogramming
import           Data.Atlas.PtEtaPhiE
import           Data.Atlas.Variation
import qualified Data.Map.Strict          as M
import           Data.Serialize
import qualified Data.Text                as T
import           Data.TTree
import           GHC.Float
import           GHC.Generics             (Generic)


data Muon =
  Muon
    { mPtEtaPhiE   :: Vars PtEtaPhiE
    , mCharge      :: Int
    , mD0Sig       :: Double
    , mPtVarCone30 :: Double
    } deriving (Show, Generic)

instance Serialize Muon

-- instance HasLorentzVector Muon where
--     toPtEtaPhiE = lens mPtEtaPhiE $ \m lv -> m { mPtEtaPhiE = lv }

readMuons :: MonadIO m => TR m [Muon]
readMuons = do
  tlvs <- lvsFromTTreeF "MuonPt" "MuonEta" "MuonPhi" "MuonE"
  let tlvs' = over (traverse.lvPt) (+100) tlvs
      ltlvs' = imap (\i v -> M.singleton (T.pack ("muvar" ++ show i)) v) tlvs'
      vtlvs = Variations <$> tlvs <*> ltlvs'
  chs <- fmap ci2i <$> readBranch "MuonCharge"
  d0sigs <- fmap float2Double <$> readBranch "MuonD0Sig"
  ptvc20s <- fmap float2Double <$> readBranch "MuonMIsol20"
  return . getZipList $ Muon <$> vtlvs <*> chs <*> d0sigs <*> ptvc20s

  where
    ci2i :: CInt -> Int
    ci2i = fromEnum

-- TODO
-- muonHs :: Foldl (Corrected SF Muon) (Folder (Vars YodaObj))
-- muonHs = lvHs

muonHs :: Foldl (Corrected SF Muon) (Vars (Folder YodaObj))
muonHs = bindF (traverse mPtEtaPhiE) lvHs
