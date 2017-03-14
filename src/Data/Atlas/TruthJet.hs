{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Atlas.TruthJet
  ( TruthJet(..), tjChargedConsts, tjBHadrons
  , readTruthJets, bhChildren
  , truthJetHs
  ) where

import           Control.Applicative      (ZipList (..))
import           Control.Lens
import           Data.Atlas.BFrag
import           Data.Atlas.Histogramming
import           Data.Atlas.PtEtaPhiE
import           Data.List                (deleteFirstsBy)
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

truthJetHs :: FillSimple TruthJet
truthJetHs = mconcat
  [ lvHs
  , bfragHs
  ]

data BHadron =
  BHadron
    { _bhPtEtaPhiE :: PtEtaPhiE
    , _bhChildren  :: [PtEtaPhiE]
    } deriving (Generic, Show)

instance HasLorentzVector BHadron where
  toPtEtaPhiE = lens _bhPtEtaPhiE $ \b x -> b { _bhPtEtaPhiE = x }

instance HasSVTracks TruthJet where
  svTracks = toListOf (tjBHadrons.traverse.bhChildren.traverse)

instance HasPVTracks TruthJet where
  pvTracks tj =
    let bchs = svTracks tj
        chparts = view tjChargedConsts tj
    in deleteFirstsBy eq chparts bchs
    where
      eq x y = lvDREta x y < 0.01

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
  let tmp = V.fromList . getZipList $ TruthJet <$> tlvs <*> chconsts <*> pure []

  bhads <- filter ((> 5) . view lvPt) <$> readBHadrons

  return . filter filt . V.toList
    $ foldr matchBTJ tmp bhads

  where
    filt j = lengthOf (tjBHadrons.traverse) j == 1 && view lvPt j > 25


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
matchBTJ :: BHadron -> V.Vector TruthJet -> V.Vector TruthJet
matchBTJ bh tjs =
  let mi = V.minIndex $ lvDREta bh <$> tjs
  in over (ix mi) (g bh) tjs

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
