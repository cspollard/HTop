{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Atlas.TruthJet
  ( TruthJet(..), tjChargedConsts, tjBHadrons
  , readTruthJets, truthJetBFrag
  -- , truthJetHs
  ) where

import           Control.Applicative      (ZipList (..))
import           Control.Lens
import           Data.Atlas.Histogramming
import           Data.Atlas.PtEtaPhiE
import           Data.Foldable            (fold)
import           Data.List                (unionBy)
import           Data.TTree
import qualified Data.Vector              as V
import           GHC.Float
import           GHC.Generics             (Generic)

data TruthJet =
  TruthJet
    { _tjPtEtaPhiE     :: PtEtaPhiE
    , _tjChargedConsts :: [PtEtaPhiE]
    , _tjBHadrons      :: [BHadron]
    } deriving (Generic, Show)

instance HasLorentzVector TruthJet where
  toPtEtaPhiE = lens _tjPtEtaPhiE $ \tj x -> tj { _tjPtEtaPhiE = x }

data BHadron =
  BHadron
    { _bhPtEtaPhiE :: PtEtaPhiE
    , _bhChildren  :: [PtEtaPhiE]
    } deriving (Generic, Show)

instance HasLorentzVector BHadron where
  toPtEtaPhiE = lens _bhPtEtaPhiE $ \b x -> b { _bhPtEtaPhiE = x }

trueBChSum :: TruthJet -> PtEtaPhiE
trueBChSum = foldOf (tjBHadrons.traverse.bhChildren.traverse)

truthJetChSum :: TruthJet -> PtEtaPhiE
truthJetChSum tj =
  let bchs = toListOf (tjBHadrons.traverse.bhChildren.traverse) tj
      chparts = view tjChargedConsts tj
  in fold $ unionBy eq bchs chparts
  where
    eq x y = lvDREta x y < 0.01

truthJetBFrag :: TruthJet -> Double
truthJetBFrag tj =
  let bchs = toListOf (tjBHadrons.traverse.bhChildren.traverse) tj
      chparts = unionBy eq bchs $ view tjChargedConsts tj
  in case view lvPt $ fold chparts of
    0.0 -> 0.0
    x   -> view lvPt (fold bchs) / x
  where
    eq x y = lvDREta x y < 0.01

-- TODO
-- HERE
-- trkSumPtH :: Fill TruthJet
-- trkSumPtH =
--   hist1DDef
--     (binD 0 25 500)
--     "$p_{\\mathrm T} \\sum \\mathrm{trk}$"
--     (dsigdXpbY pt gev)
--     "/trksumpt"
--     <$= view (trkSum.lvPt)
--
-- truthJetHs = mconcat
--     [ lvHs
--     , trkSumPtH
--     , bTrkSumPtH
--     , bFragH
--     , trkSumPtProfJetPt
--     , svTrkSumPtProfJetPt
--     , bFragProfJetPt
--     , trkSumPtProfJetEta
--     , svTrkSumPtProfJetEta
--     , bFragProfJetEta
--     , nPVTrksH
--     , nSVTrksH
--     , nPVTrksProfJetPt
--     , nSVTrksProfJetPt
--     , bFragVsJetPt
--     ]

readBHadrons :: MonadIO m => TR m [BHadron]
readBHadrons = do
  tlvs <-
    lvsFromTTreeF
      "TruthBhadPt"
      "TruthBhadEta"
      "TruthBhadPhi"
      "TruthBhadE"

  chtlvs <-
    vecVecTLV
      "TruthBhadChiPt"
      "TruthBhadChiEta"
      "TruthBhadChiPhi"
      "TruthBhadChiE"

  return . getZipList $ BHadron <$> tlvs <*> chtlvs

readTruthJets :: MonadIO m => TR m [TruthJet]
readTruthJets = do
  tlvs <- lvsFromTTreeF "TruthJetPt" "TruthJetEta" "TruthJetPhi" "TruthJetE"
  chconsts <-
    vecVecTLV "TruthJetChPt" "TruthJetChEta" "TruthJetChPhi" "TruthJetChE"
  let tmp = getZipList $ TruthJet <$> tlvs <*> chconsts <*> pure []

  bhads <- readBHadrons

  return $ foldr matchBTJ tmp bhads


vecVecTLV
  :: MonadIO m
  => String -> String -> String -> String -> TR m (ZipList [PtEtaPhiE])
vecVecTLV spt seta sphi se = do
    partpts <- (fmap.fmap) float2Double . fromVVector <$> readBranch spt
    partetas <- (fmap.fmap) float2Double . fromVVector <$> readBranch seta
    partphis <- (fmap.fmap) float2Double . fromVVector <$> readBranch sphi
    partes <- (fmap.fmap) float2Double . fromVVector <$> readBranch se

    let ps = V.zipWith4
            ( \pts etas phis es ->
                V.toList $ V.zipWith4 PtEtaPhiE pts etas phis es
            ) partpts partetas partphis partes

    return . ZipList $ V.toList ps


-- TODO
-- better matching criterion?
matchBTJ :: BHadron -> [TruthJet] -> [TruthJet]
matchBTJ bh tjs =
  let vtjs = V.fromList tjs
      mi = V.minIndex $ lvDREta bh <$> vtjs
  in V.toList $ over (ix mi) (g bh) vtjs

  where
    g b j =
      if lvDREta b j < 0.3
        then over tjBHadrons ((:) b) j
        else j


tjChargedConsts :: Lens' TruthJet [PtEtaPhiE]
tjChargedConsts = lens _tjChargedConsts $ \tj x -> tj { _tjChargedConsts = x }

tjBHadrons :: Lens' TruthJet [BHadron]
tjBHadrons = lens _tjBHadrons $ \tj x -> tj { _tjBHadrons = x }

bhChildren :: Lens' BHadron [PtEtaPhiE]
bhChildren = lens _bhChildren $ \b x -> b { _bhChildren = x }
