{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module BFrag.BFrag where

import           Atlas
import           Control.Lens
import           Data.Foldable  (fold)
import           Data.Semigroup


class HasSVTracks a where
  svTracks :: a -> PhysObj [PtEtaPhiE]

svTrackSum :: HasSVTracks a => a -> PhysObj PtEtaPhiE
svTrackSum = fmap fold . svTracks

svTrackSumPt :: HasSVTracks a => a -> PhysObj Double
svTrackSumPt = fmap (view lvPt) . svTrackSum

nSVTracks :: HasSVTracks a => a -> PhysObj Int
nSVTracks = fmap length . svTracks


class HasPVTracks a where
  pvTracks :: a -> PhysObj [PtEtaPhiE]

pvTrackSum :: HasPVTracks a => a -> PhysObj PtEtaPhiE
pvTrackSum = fmap fold . pvTracks

pvTrackSumPt :: HasPVTracks a => a -> PhysObj Double
pvTrackSumPt = fmap (view lvPt) . pvTrackSum

nPVTracks :: HasPVTracks a => a -> PhysObj Int
nPVTracks = fmap length . pvTracks

trackSumPt :: (HasSVTracks a, HasPVTracks a) => a -> PhysObj Double
trackSumPt j = do
  svt <- svTracks j
  pvt <- pvTracks j
  return . view lvPt . fold $ svt ++ pvt


zBTCharged, zBL
  :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a)
  => a -> PhysObj Double
zBTCharged j = do
  svt <- svTracks j
  pvt <- pvTracks j
  let svtsum = fold svt
  return $
    case view lvPt . foldr (<>) svtsum $ pvt of
      0.0 -> 0.0
      x   -> view lvPt svtsum / x


-- TODO!!!
zBL = undefined


trkSumPtH
  :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a)
  => Foldl (PhysObj a) (Vars YodaObj)
trkSumPtH =
  physObjH h =$<< trackSumPt

  where
    h =
      hist1DDef
        (binD 0 25 250)
        "$p_{\\mathrm T} \\sum \\mathrm{trk}$"
        (dndx pt gev)


svTrkSumPtH
  :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a)
  => Foldl (PhysObj a) (Vars YodaObj)
svTrkSumPtH =
  physObjH h =$<< svTrackSumPt

  where
    h =
      hist1DDef
        (binD 0 25 250)
        "$p_{\\mathrm T} \\sum \\mathrm{SV trk}$"
        (dndx pt gev)


zBTChargedH
  :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a)
  => Foldl (PhysObj a) (Vars YodaObj)
zBTChargedH =
  physObjH h =$<< zBTCharged

  where
    h =
      hist1DDef
        (binD 0 7 1.05)
        "$z_{p_{\\mathrm T}}$"
        (dndx "z_{p_{\\mathrm T}}" "1")


nPVTrksH
  :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a)
  => Foldl (PhysObj a) (Vars YodaObj)
nPVTrksH =
  physObjH h =$<< fmap (fromIntegral . length) . pvTracks

  where
    h =
      hist1DDef
        (binD 0 20 20)
        "$n$ PV tracks"
        (dndx "n" "1")


nSVTrksH
  :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a)
  => Foldl (PhysObj a) (Vars YodaObj)
nSVTrksH =
  physObjH h =$<< fmap fromIntegral . nSVTracks

  where
    h =
      hist1DDef
        (binD 0 20 20)
        "$n$ SV tracks"
        (dndx "n" "1")


bfragHs
  :: (HasPVTracks a, HasSVTracks a, HasLorentzVector a)
  => Fills a
bfragHs =
  mconcat
  [ singleton "/zbt" <$> zBTChargedH
  , singleton "/trksumpt" <$> trkSumPtH
  , singleton "/svtrksumpt" <$> svTrkSumPtH
  , singleton "/npvtrks" <$> nPVTrksH
  , singleton "/nsvtrks" <$> nSVTrksH
  , lvHs

--     -- , trkSumPtProfPt
--     -- , svTrkSumPtProfPt
--     -- , zBTChargedProfPt
--     -- , trkSumPtProfEta
--     -- , svTrkSumPtProfEta
--     -- , zBTChargedProfEta
--     -- , nPVTrksProfPt
--     -- , nSVTrksProfPt
--     -- , zBTChargedVsPt
  ]


-- trkSumPtProfPt
--   :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a)
--   => Foldl (a, Double) YodaObj
-- trkSumPtProfPt =
--   fmap (singleton "/trksumptprofpt")
--   $ prof1DDef
--     (binD 25 18 250)
--     "$p_{\\mathrm T}$ [GeV]"
--     "$<p_{\\mathrm T} \\sum \\mathrm{trk}>$"
--     <$= first (view lvPt &&& trackSumPt)
--
--
-- svTrkSumPtProfPt
--   :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a)
--   => Foldl (a, Double) YodaObj
-- svTrkSumPtProfPt =
--   fmap (singleton "/svtrksumptprofpt")
--   $  prof1DDef
--     (binD 25 18 250)
--     "$p_{\\mathrm T}$ [GeV]"
--     "$<p_{\\mathrm T} \\sum \\mathrm{SV trk}>$"
--     <$= first (view lvPt &&& svTrackSumPt)
--
--
-- zBTChargedVsPt
--   :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a)
--   => Foldl (a, Double) YodaObj
-- zBTChargedVsPt =
--   fmap (singleton "/zbtvspt")
--   $ hist2DDef
--     (binD 25 18 250)
--     (binD 0 21 1.05)
--     "$p_{\\mathrm T}$ [GeV]"
--     "$z_{p_{\\mathrm T}}$"
--     <$= first (view lvPt &&& zBTCharged)
--
--
-- zBTChargedProfPt
--   :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a)
--   => Foldl (a, Double) YodaObj
-- zBTChargedProfPt =
--   fmap (singleton "/zbtprofpt")
--   $ prof1DDef
--     (binD 25 18 250)
--     "$p_{\\mathrm T}$ [GeV]"
--     "$<z_{p_{\\mathrm T}}>$"
--     <$= first (view lvPt &&& zBTCharged)
--
--
-- trkSumPtProfEta
--   :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a)
--   => Foldl (a, Double) YodaObj
-- trkSumPtProfEta =
--   fmap (singleton "/trksumptprofeta")
--   $ prof1DDef
--     (binD 0 21 2.1)
--     "$\\eta$"
--     "$<p_{\\mathrm T} \\sum \\mathrm{trk}>$"
--     <$= first (view lvEta &&& trackSumPt)
--
-- svTrkSumPtProfEta
--   :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a)
--   => Foldl (a, Double) YodaObj
-- svTrkSumPtProfEta =
--   fmap (singleton "/svtrksumptprofeta")
--   $ prof1DDef
--     (binD 0 21 2.1)
--     "$\\eta$"
--     "$<p_{\\mathrm T} \\sum \\mathrm{SV trk}>$"
--     <$= first (view lvEta &&& svTrackSumPt)
--
--
-- zBTChargedProfEta
--   :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a)
--   => Foldl (a, Double) YodaObj
-- zBTChargedProfEta =
--   fmap (singleton "/zbtprofeta")
--   $ prof1DDef
--     (binD 0 21 2.1)
--     "$\\eta$"
--     "$<z_{p_{\\mathrm T}}>$"
--     <$= first (view lvEta &&& zBTCharged)
--
--
-- svTrkSumPtProfTrkSumPt
--   :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a)
--   => Foldl (a, Double) YodaObj
-- svTrkSumPtProfTrkSumPt =
--   fmap (singleton "/svtrksumptproftrksumpt")
--   $ prof1DDef
--     (binD 0 25 100)
--     "$p_{\\mathrm T} \\sum \\mathrm{trk}$"
--     "$<p_{\\mathrm T} \\sum \\mathrm{SV trk}>$"
--     <$= first (svTrackSumPt &&& trackSumPt)
--
--
-- zBTChargedProfTrkSumPt
--   :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a)
--   => Foldl (a, Double) YodaObj
-- zBTChargedProfTrkSumPt =
--   fmap (singleton "/zbtproftrksumpt")
--   $ prof1DDef
--     (binD 0 25 100)
--     "$p_{\\mathrm T} \\sum \\mathrm{trk}$"
--     "$<z_{p_{\\mathrm T}}>$"
--     <$= first (trackSumPt &&& zBTCharged)
--
--
--
-- nPVTrksProfPt
--   :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a)
--   => Foldl (a, Double) YodaObj
-- nPVTrksProfPt =
--   fmap (singleton "/npvtrksprofpt")
--   $ prof1DDef
--     (binD 25 18 250)
--     "$p_{\\mathrm T}$ [GeV]"
--     "$<n$ PV tracks $>$"
--     <$= first (view lvPt &&& (fromIntegral . nPVTracks))
--
--
-- nSVTrksProfPt
--   :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a)
--   => Foldl (a, Double) YodaObj
-- nSVTrksProfPt =
--   fmap (singleton "/nsvtrksprofpt")
--   $ prof1DDef
--     (binD 25 18 250)
--     "$p_{\\mathrm T}$ [GeV]"
--     "$<n$ SV tracks $>$"
--     <$= first (view lvPt &&& (fromIntegral . nSVTracks))
