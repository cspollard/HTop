{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module BFrag.TrueJet
  ( TrueJet(..), tjChargedConsts, tjBHadrons
  , readTrueJets, bhChargedChildren
  , trueBJet, svTrue
  ) where

import           Atlas
import           BFrag.BFrag
import           BFrag.PtEtaPhiE
import           Control.Applicative (ZipList (..))
import           Control.Lens
import           Data.Foldable       (fold)
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
    { _bhPtEtaPhiE       :: PtEtaPhiE
    , _bhChargedChildren :: [PtEtaPhiE]
    } deriving (Generic, Show)


instance HasLorentzVector BHadron where
  toPtEtaPhiE = lens _bhPtEtaPhiE $ \b x -> b { _bhPtEtaPhiE = x }

instance HasSVConstits TrueJet where
  svConstits = pure . toListOf (tjBHadrons.traverse.toPtEtaPhiE)
  svChargedConstits = pure . toListOf (tjBHadrons.traverse.bhChargedChildren.traverse)

instance HasPVConstits TrueJet where
  pvConstits tj = pure . lvDiff (_tjPtEtaPhiE tj) . fold <$> svConstits tj
    where
      lvDiff x y = x `mappend` lvNegate y

  pvChargedConstits = pure . _tjChargedConsts


svTrue :: TrueJet -> PtEtaPhiE
svTrue = foldOf $ tjBHadrons . traverse . toPtEtaPhiE

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
  -- let chtlvs = pure []

  chtlvs <-
    vecVecTLV
      "bhad_child_pt"
      "bhad_child_eta"
      "bhad_child_phi"
      "bhad_child_e"

  return . getZipList $ BHadron <$> tlvs <*> chtlvs

readTrueJets :: (MonadIO m, MonadThrow m) => TreeRead m [TrueJet]
readTrueJets = do
  tlvs <- lvsFromTTreeF "jet_pt" "jet_eta" "jet_phi" "jet_e"
  chconsts <-
    vecVecTLV "jet_track_pt" "jet_track_eta" "jet_track_phi" "jet_track_e"
  let tmp = V.fromList . getZipList $ TrueJet <$> tlvs <*> chconsts <*> pure []

  bhads <- filter ((> 5) . view lvPt) <$> readBHadrons

  return . V.toList
    $ foldr matchBTJ tmp bhads


trueBJet :: TrueJet -> Bool
trueBJet j = lengthOf (tjBHadrons.traverse) j == 1 && view lvPt j > 25


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

bhChargedChildren :: Lens' BHadron [PtEtaPhiE]
bhChargedChildren = lens _bhChargedChildren $ \b x -> b { _bhChargedChildren = x }
