{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE RankNTypes #-}

module Data.Atlas.Jet where

import Control.Lens

import GHC.Float
import GHC.Generics (Generic)

import Control.Applicative (ZipList(..))
import Data.Foldable (fold)
import Data.Monoid ((<>))
import Data.List (deleteFirstsBy)

import qualified Data.Vector as V

import Data.HEP.LorentzVector
import Data.TTree
import Data.Atlas.PtEtaPhiE
import Data.Atlas.DataMC

data Jet a =
    Jet
        { _jPtEtaPhiE :: PtEtaPhiE
        , _jMV2c10 :: Double
        , _jJVT :: Double
        , _jPVTracks :: [PtEtaPhiE]
        , _jSVTracks :: [PtEtaPhiE]
        , _jMCInfo :: MCInfo (Jet a)
        } deriving Generic


instance Show (Jet a) where
    show j = "Jet " ++ views toPtEtaPhiE show j

-- TODO
-- macro here?
-- can't use template haskell
jJVT, jMV2c10 :: Lens' (Jet a) Double
jJVT = lens _jJVT $ \j x -> j { _jJVT = x }
jMV2c10 = lens _jMV2c10 $ \j x -> j { _jMV2c10 = x }


jPVTracks, jSVTracks :: Lens' (Jet a) [PtEtaPhiE]
jPVTracks = lens _jPVTracks $ \j x -> j { _jPVTracks = x }
jSVTracks = lens _jSVTracks $ \j x -> j { _jSVTracks = x }

jMCInfo :: Lens' (Jet a) (MCInfo (Jet a))
jMCInfo = lens _jMCInfo $ \j x -> j { _jMCInfo = x }

instance HasLorentzVector (Jet a) where
    toPtEtaPhiE = lens _jPtEtaPhiE $ \j x -> j { _jPtEtaPhiE = x }

instance HasMCInfo (Jet MC) where
    type MCInfo (Jet MC) = CInt
    mcInfo = jMCInfo

instance HasMCInfo (Jet Data') where
    type MCInfo (Jet Data') = ()
    mcInfo = jMCInfo


-- TODO
-- MonoFoldable?
newtype Jets a = Jets { fromJets :: [Jet a] } deriving Generic

jetTracksIsTight :: MonadIO m => TR m [[Bool]]
jetTracksIsTight = V.toList . fmap V.toList .  over (traverse.traverse) (/= (0 :: CInt)) . fromVVector <$> readBranch "JetTracksisTight"


-- Generic jet (without extra info)
jetFromTTreeG :: MonadIO m => TR m ([MCInfo (Jet a)] -> Jets a)
jetFromTTreeG = do
    PtEtaPhiEs tlvs <- lvsFromTTree "JetPt" "JetEta" "JetPhi" "JetE"
    mv2c10s <- fmap float2Double <$> readBranch "JetMV2c20"
    jvts <- fmap float2Double <$> readBranch "JetJVT"
    trks <- jetTracksTLV "JetTracksPt" "JetTracksEta" "JetTracksPhi" "JetTracksE"
    trksTight <- jetTracksIsTight

    let trks' = fmap snd . filter fst <$> zipWith zip trksTight trks

    sv1trks <- jetTracksTLV "JetSV1TracksPt" "JetSV1TracksEta" "JetSV1TracksPhi" "JetSV1TracksE"

    let trks'' = zipWith (deleteFirstsBy trkEq) trks' sv1trks

    let js = Jet <$> ZipList tlvs <*> mv2c10s <*> jvts <*> ZipList trks'' <*> ZipList sv1trks
    return $ \eis -> (Jets . getZipList) (js <*> ZipList eis)


instance FromTTree (Jets MC) where
    fromTTree = jetFromTTreeG <*> readBranch "JetTruthLabel"

instance FromTTree (Jets Data') where
    fromTTree = jetFromTTreeG <*> return (repeat ())


pvTrkSumTLV :: Jet a -> PtEtaPhiE
pvTrkSumTLV = fold . view jPVTracks

svTrkSumTLV :: Jet a -> PtEtaPhiE
svTrkSumTLV = fold . view jSVTracks

trkSumTLV :: Jet a -> PtEtaPhiE
trkSumTLV = (<>) <$> svTrkSumTLV <*> pvTrkSumTLV

pvTrkSumPt :: Jet a -> Double
pvTrkSumPt = view lvPt . pvTrkSumTLV

svTrkSumPt :: Jet a -> Double
svTrkSumPt = view lvPt . svTrkSumTLV

trkSumPt :: Jet a -> Double
trkSumPt = view lvPt . trkSumTLV

-- protect against dividing by zero
bFrag :: Jet a -> Maybe Double
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
p `trkEq` p' = (p `lvDREta` p') < 0.005
