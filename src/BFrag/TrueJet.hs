{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module BFrag.TrueJet
  ( TrueJet(..), tjChargedConsts, tjBHadrons
  , readTrueJets, bhChildren, zBTTrue
  ) where

import           Atlas
import           BFrag.BFrag
import           BFrag.PtEtaPhiE
import           Control.Applicative (ZipList (..))
import           Control.Lens
import           Data.List           (deleteFirstsBy)
import           Data.TTree
import qualified Data.Vector         as V
import           GHC.Float
import           GHC.Generics        (Generic)

data TrueJet =
  TrueJet
    { _tjPtEtaPhiE     :: PtEtaPhiE
    , _tjChargedConsts :: [PtEtaPhiE]
    , _tjBHadrons      :: [BHadron]
    } deriving (Generic, Show)

instance HasLorentzVector TrueJet where
  toPtEtaPhiE = lens _tjPtEtaPhiE $ \tj x -> tj { _tjPtEtaPhiE = x }

data BHadron =
  BHadron
    { _bhPtEtaPhiE :: PtEtaPhiE
    , _bhChildren  :: [PtEtaPhiE]
    } deriving (Generic, Show)


instance HasLorentzVector BHadron where
  toPtEtaPhiE = lens _bhPtEtaPhiE $ \b x -> b { _bhPtEtaPhiE = x }

instance HasSVTracks TrueJet where
  svTracks = toListOf (tjBHadrons.traverse.bhChildren.traverse)

instance HasPVTracks TrueJet where
  pvTracks tj =
    let bchs = svTracks tj
        chparts = view tjChargedConsts tj
    in deleteFirstsBy eq chparts bchs
    where
      eq x y = lvDREta x y < 0.01


zBTTrue :: TrueJet -> Double
zBTTrue (TrueJet tlv _ bs) =
  view lvPt (foldOf (traverse.toPtEtaPhiE) bs) / view lvPt tlv


readBHadrons :: (MonadIO m, MonadThrow m) => TreeRead m [BHadron]
readBHadrons = do
  tlvs <-
    lvsFromTTreeF
      "bhad_pt"
      "bhad_eta"
      "bhad_phi"
      "bhad_e"

  -- TODO
  -- these are broken in ntuples
  let chtlvs = pure []

  -- chtlvs <-
  --   vecVecTLV
  --     "bhad_child_pt"
  --     "bhad_child_eta"
  --     "bhad_child_phi"
  --     "bhad_child_e"

  return . getZipList $ BHadron <$> tlvs <*> chtlvs

readTrueJets :: (MonadIO m, MonadThrow m) => TreeRead m [TrueJet]
readTrueJets = do
  tlvs <- lvsFromTTreeF "jet_pt" "jet_eta" "jet_phi" "jet_e"
  chconsts <-
    vecVecTLV "jet_track_pt" "jet_track_eta" "jet_track_phi" "jet_track_e"
  let tmp = V.fromList . getZipList $ TrueJet <$> tlvs <*> chconsts <*> pure []

  bhads <- filter ((> 5) . view lvPt) <$> readBHadrons

  -- TODO
  -- we're not filtering
  -- return . filter filt . V.toList
  return . V.toList
    $ foldr matchBTJ tmp bhads

  -- where
    -- filt j = lengthOf (tjBHadrons.traverse) j == 1 && view lvPt j > 25


vecVecTLV
  :: (MonadIO m, MonadThrow m)
  => String -> String -> String -> String -> TreeRead m (ZipList [PtEtaPhiE])
vecVecTLV spt seta sphi se = do
    partpts <- (fmap.fmap) ((/1e3) . float2Double) . fromVVector <$> readBranch spt
    partetas <- (fmap.fmap) float2Double . fromVVector <$> readBranch seta
    partphis <- (fmap.fmap) float2Double . fromVVector <$> readBranch sphi
    partes <- (fmap.fmap) ((/1e3) . float2Double) . fromVVector <$> readBranch se

    let ps = V.zipWith4
            ( \pts etas phis es ->
                V.toList $ V.zipWith4 PtEtaPhiE pts etas phis es
            ) partpts partetas partphis partes

    return . ZipList $ V.toList ps


-- TODO
-- better matching criterion?
matchBTJ :: BHadron -> V.Vector TrueJet -> V.Vector TrueJet
matchBTJ bh tjs =
  if V.length tjs == 0
    then tjs
    else
      let mi = V.minIndex $ lvDREta bh <$> tjs
      in over (ix mi) (g bh) tjs

  where
    g b j =
      if lvDREta b j < 0.3
        then over tjBHadrons ((:) b) j
        else j


tjChargedConsts :: Lens' TrueJet [PtEtaPhiE]
tjChargedConsts = lens _tjChargedConsts $ \tj x -> tj { _tjChargedConsts = x }

tjBHadrons :: Lens' TrueJet [BHadron]
tjBHadrons = lens _tjBHadrons $ \tj x -> tj { _tjBHadrons = x }

bhChildren :: Lens' BHadron [PtEtaPhiE]
bhChildren = lens _bhChildren $ \b x -> b { _bhChildren = x }
