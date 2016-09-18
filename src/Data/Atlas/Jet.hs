{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Atlas.Jet where

import Control.Lens

import GHC.Generics (Generic)
import Data.Serialize

import Control.Applicative (ZipList(..))
import Data.Foldable (fold)
import Data.Monoid ((<>))
import Data.List (deleteFirstsBy)

import Data.HEP.LorentzVector
import Data.Atlas.PtEtaPhiE
import Data.TTree

import qualified Data.Vector as V

data Jet =
    Jet
        { jPtEtaPhiE :: PtEtaPhiE
        , jMV2c10 :: Float
        , jJVT :: Float
        , jPVTracks :: [PtEtaPhiE]
        , jSVTracks :: [PtEtaPhiE]
        } deriving (Show, Generic)

instance Serialize Jet

instance HasLorentzVector Jet where
    toPtEtaPhiE = lens jPtEtaPhiE $ \j lv -> j { jPtEtaPhiE = lv }

data JetTruthLabel =
    BLabel
    | CLabel
    | TauLabel
    | LLabel
    | NoLabel

instance FromTTree JetTruthLabel where
    fromTTree = do l <- readBranch "

newtype Jets = Jets { fromJets :: [Jet] } deriving (Show, Generic, Serialize)

jetTracksIsTight :: MonadIO m => TR m [[Bool]]
jetTracksIsTight = V.toList . fmap V.toList .  over (traverse.traverse) (/= (0 :: CInt)) . fromVVector <$> readBranch "JetTracksisTight"


instance FromTTree Jets where
    fromTTree = do
        PtEtaPhiEs tlvs <- lvsFromTTree "JetPt" "JetEta" "JetPhi" "JetE"
        mv2c10s <- readBranch "JetMV2c20"
        jvts <- readBranch "JetJVT"
        trks <- jetTracksTLV "JetTracksPt" "JetTracksEta" "JetTracksPhi" "JetTracksE"
        trksTight <- jetTracksIsTight

        let trks' = fmap snd . filter fst <$> zipWith zip trksTight trks

        sv1trks <- jetTracksTLV "JetSV1TracksPt" "JetSV1TracksEta" "JetSV1TracksPhi" "JetSV1TracksE"

        let trks'' = zipWith (deleteFirstsBy trkEq) trks' sv1trks

        let js = Jet <$> ZipList tlvs <*> mv2c10s <*> jvts <*> ZipList trks'' <*> ZipList sv1trks
        return . Jets $ getZipList js


pvTrkSumTLV :: Jet -> PtEtaPhiE
pvTrkSumTLV = fold . jPVTracks

svTrkSumTLV :: Jet -> PtEtaPhiE
svTrkSumTLV = fold . jSVTracks

trkSumTLV :: Jet -> PtEtaPhiE
trkSumTLV = (<>) <$> svTrkSumTLV <*> pvTrkSumTLV

pvTrkSumPt :: Jet -> Double
pvTrkSumPt = view lvPt . pvTrkSumTLV

svTrkSumPt :: Jet -> Double
svTrkSumPt = view lvPt . svTrkSumTLV

trkSumPt :: Jet -> Double
trkSumPt = view lvPt . trkSumTLV

-- protect against dividing by zero
bFrag :: Jet -> Maybe Double
bFrag j = case trkSumPt j of
               0.0 -> Nothing
               x   -> Just (svTrkSumPt j / x)


jetTracksTLV :: MonadIO m
             => String -> String -> String -> String -> TR m [[PtEtaPhiE]]
jetTracksTLV spt seta sphi se = do
    trkpts <- fromVVector <$> readBranch spt
    trketas <- fromVVector <$> readBranch seta
    trkphis <- fromVVector <$> readBranch sphi
    trkes <- fromVVector <$> readBranch se

    let trks = V.zipWith4
            ( \pts etas phis es ->
                V.toList $ V.zipWith4 PtEtaPhiE pts etas phis es
            ) trkpts trketas trkphis trkes

    return $ V.toList trks

trkEq :: PtEtaPhiE -> PtEtaPhiE -> Bool
p `trkEq` p' = (p `lvDR` p') < 0.005

{-
 - TODO
 - other jet collections

data LargeJet = LargeJet { ljPtEtaPhiE :: PtEtaPhiE
                         , ljM :: Float
                         , ljSD12 :: Float
                         , ljTau21 :: Float
                         , ljTau32 :: Float
                         -- TODO
                         -- can't handle vec<vec<int>> yet
                         -- , ljGhostTJs :: [Int]
                         } deriving (Show, Generic)


instance Serialize LargeJet

instance HasLorentzVector LargeJet where
    lv = fromLV . ljPtEtaPhiE

newtype LargeJets = LargeJets [LargeJet]

instance FromTTree LargeJets where
    fromTTree = do PtEtaPhiEs tlvs <- lvsFromTTree "ljet"
                   sd12s <- readBranch "ljet_sd12"
                   sd12s <- readBranch "ljet_sd12"
                   tau21s <- readBranch "ljet_tau21"
                   tau32s <- readBranch "ljet_tau32"
                   let js = LargeJet <$> ZipList tlvs <*> sd12s <*> tau21s <*> tau32s
                   return . LargeJets . getZipList $ js


data TrackJet = TrackJet { tjPtEtaPhiE :: PtEtaPhiE
                         , tjMV2c10 :: Float
                         -- TODO
                         -- this won't be present in data.
                         -- tjLabel :: Maybe Int
                         } deriving (Show, Generic)

instance Serialize TrackJet

instance HasLorentzVector TrackJet where
    lv = fromLV . tjPtEtaPhiE

newtype TrackJets = TrackJets [TrackJet]

instance FromTTree TrackJets where
    fromTTree = do PtEtaPhiE tlvs <- lvsFromTTree "tjet"
                   mv2c10s <- readBranch "tjet_mv2c10"
                   js <- TrackJet <$> ZipList tlvs <*> mv2c10s
                   return . TrackJets . getZipList $ js
-}
