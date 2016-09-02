{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Atlas.Jet where

import Control.Lens

import GHC.Generics (Generic)
import Data.Serialize
import Control.Applicative (ZipList(..))

import Data.HEP.LorentzVector
import Data.Atlas.PtEtaPhiE
import Data.TTree

import qualified Data.Vector as V

data Jet = Jet { jPtEtaPhiE :: PtEtaPhiE
               , jMV2c10 :: Float
               , jJVT :: Float
               , jTracks :: [PtEtaPhiE]
               , jSV1Tracks :: [PtEtaPhiE]
               } deriving (Show, Generic)

instance Serialize Jet

instance HasLorentzVector Jet where
    lv = fromLV . jPtEtaPhiE

newtype Jets = Jets { fromJets :: [Jet] } deriving (Show, Generic, Serialize)

jetTracksTLV :: MonadIO m => String -> String -> String -> String -> TR m [[PtEtaPhiE]]
jetTracksTLV spt seta sphi se = do trkpts <- fromVVector <$> readBranch spt
                                   trketas <- fromVVector <$> readBranch seta
                                   trkphis <- fromVVector <$> readBranch sphi
                                   trkes <- fromVVector <$> readBranch se

                                   let trks = V.zipWith4 (\pts etas phis es ->
                                                           V.toList $ V.zipWith4 PtEtaPhiE pts etas phis es
                                                          ) trkpts trketas trkphis trkes

                                   return $ V.toList trks


jetTracksIsTight :: MonadIO m => TR m [[Bool]]
jetTracksIsTight = V.toList . fmap V.toList . over (traverse.traverse) ((/= 0) :: CInt -> Bool) . fromVVector <$> readBranch "JetTracksisTight"


instance FromTTree Jets where
    fromTTree = do PtEtaPhiEs tlvs <- lvsFromTTree "JetPt" "JetEta" "JetPhi" "JetE"
                   mv2c10s <- readBranch "JetMV2c20"
                   jvts <- readBranch "JetJVT"
                   trks <- jetTracksTLV "JetTracksPt" "JetTracksEta" "JetTracksPhi" "JetTracksE"
                   trksTight <- jetTracksIsTight

                   let trks' = fmap snd . filter fst <$> zipWith zip trksTight trks

                   sv1trks <- jetTracksTLV "JetSV1TracksPt" "JetSV1TracksEta" "JetSV1TracksPhi" "JetSV1TracksE"

                   let js = Jet <$> ZipList tlvs <*> mv2c10s <*> jvts <*> ZipList trks' <*> ZipList sv1trks
                   return . Jets $ getZipList js

sumTrkPt :: Jet -> Double
sumTrkPt = sum . fmap lvPt . jTracks

sumSV1TrkPt :: Jet -> Double
sumSV1TrkPt = sum . fmap lvPt . jSV1Tracks

bFrag :: Jet -> Double
bFrag = (/) <$> sumSV1TrkPt <*> sumTrkPt

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
