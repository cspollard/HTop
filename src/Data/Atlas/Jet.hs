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
import Data.List (deleteFirstsBy)

import qualified Data.Vector as V

import Data.HEP.LorentzVector
import Data.TTree
import Data.Atlas.PtEtaPhiE
import Data.Atlas.DataMC

data Jet a =
    Jet
        { _jPtEtaPhiE :: PtEtaPhiE
        , _mv2c10 :: Double
        , _jvt :: Double
        , _tracks :: [PtEtaPhiE]
        , _pvTracks :: [PtEtaPhiE]
        , _svTracks :: [PtEtaPhiE]
        , _trkSum :: PtEtaPhiE
        , _pvTrkSum :: PtEtaPhiE
        , _svTrkSum :: PtEtaPhiE
        , _jMCInfo :: MCInfo (Jet a)
        } deriving Generic


instance Show (Jet a) where
    show j = "Jet " ++ views toPtEtaPhiE show j

-- TODO
-- macro here?
-- can't use template haskell
jvt, mv2c10 :: Lens' (Jet a) Double
jvt = lens _jvt $ \j x -> j { _jvt = x }
mv2c10 = lens _mv2c10 $ \j x -> j { _mv2c10 = x }


tracks, pvTracks, svTracks :: Lens' (Jet a) [PtEtaPhiE]
tracks = lens _tracks $ \j x -> j { _tracks = x }
pvTracks = lens _pvTracks $ \j x -> j { _pvTracks = x }
svTracks = lens _svTracks $ \j x -> j { _svTracks = x }

trkSum, pvTrkSum, svTrkSum :: Lens' (Jet a) PtEtaPhiE
trkSum = lens _trkSum $ \j x -> j { _trkSum = x }
pvTrkSum = lens _pvTrkSum $ \j x -> j { _pvTrkSum = x }
svTrkSum = lens _svTrkSum $ \j x -> j { _svTrkSum = x }

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

    let pvtrks = zipWith (deleteFirstsBy trkEq) trks' sv1trks
    let trks'' = zipWith (++) sv1trks pvtrks
    let trksum = map fold trks
    let pvtrksum = map fold pvtrks
    let svtrksum = map fold sv1trks

    let js = Jet
                <$> ZipList tlvs
                <*> mv2c10s
                <*> jvts
                <*> ZipList trks''
                <*> ZipList pvtrks
                <*> ZipList sv1trks
                <*> ZipList trksum
                <*> ZipList pvtrksum
                <*> ZipList svtrksum

    return $ \eis -> (Jets . getZipList) (js <*> ZipList eis)


instance FromTTree (Jets MC) where
    fromTTree = jetFromTTreeG <*> readBranch "JetTruthLabel"

instance FromTTree (Jets Data') where
    fromTTree = jetFromTTreeG <*> return (repeat ())


-- protect against dividing by zero
bFrag :: Getter (Jet a) (Maybe Double)
bFrag = to $ \j -> case view (trkSum.lvPt) j of
                    0.0 -> Nothing
                    x   -> Just (view (svTrkSum.lvPt) j / x)


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
