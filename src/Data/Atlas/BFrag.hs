{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Data.Atlas.BFrag where

import           Control.Arrow            ((&&&))
import           Control.Lens
import           Data.Atlas.Histogramming
import           Data.Foldable            (fold)
import           Data.Semigroup

class HasSVTracks a where
  svTracks :: a -> [PtEtaPhiE]

svTrackSum :: HasSVTracks a => a -> PtEtaPhiE
svTrackSum = fold . svTracks

svTrackSumPt :: HasSVTracks a => a -> Double
svTrackSumPt = view lvPt . svTrackSum


class HasPVTracks a where
  pvTracks :: a -> [PtEtaPhiE]

pvTrackSum :: HasPVTracks a => a -> PtEtaPhiE
pvTrackSum = fold . pvTracks

pvTrackSumPt :: HasPVTracks a => a -> Double
pvTrackSumPt = view lvPt . pvTrackSum

trackSumPt :: (HasSVTracks a, HasPVTracks a) => a -> Double
trackSumPt j = view lvPt . fold $ svTracks j ++ pvTracks j

zBT, zBL
  :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a)
  => a -> Double
zBT j =
  let svtrksum = fold $ svTracks j
      trksum = svtrksum <> fold (pvTracks j)
  in case view lvPt trksum of
    0.0 -> 0.0
    x   -> view lvPt svtrksum / x

-- TODO!!!
zBL = undefined

trkSumPtH :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a) => Fill a
trkSumPtH =
  hist1DDef
    (binD 0 25 500)
    "$p_{\\mathrm T} \\sum \\mathrm{trk}$"
    (dsigdXpbY pt gev)
    "/trksumpt"
    <$= trackSumPt

svTrkSumPtH :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a) => Fill a
svTrkSumPtH =
  hist1DDef
    (binD 0 25 500)
    "$p_{\\mathrm T} \\sum \\mathrm{SV trk}$"
    (dsigdXpbY pt gev)
    "/svtrksumpt"
    <$= svTrackSumPt

zBTH :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a) => Fill a
zBTH =
  hist1DDef
    (binD 0 22 1.1)
    "$z_{p_{\\mathrm T}}$"
    (dsigdXpbY "z_{p_{\\mathrm T}}" "1")
    "/zbt"
    <$= zBT

tupGetter :: Getter s a -> Getter s b -> Getter s (a, b)
tupGetter f g = runGetter ((,) <$> Getter f <*> Getter g)

trkSumPtProfPt :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a) => Fill a
trkSumPtProfPt =
  prof1DDef
    (binD 25 18 250)
    "$p_{\\mathrm T}$ [GeV]"
    "$<p_{\\mathrm T} \\sum \\mathrm{trk}>$"
    "/trksumptprofpt"
    <$= (view lvPt &&& trackSumPt)

svTrkSumPtProfPt :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a) => Fill a
svTrkSumPtProfPt =
  prof1DDef
    (binD 25 18 250)
    "$p_{\\mathrm T}$ [GeV]"
    "$<p_{\\mathrm T} \\sum \\mathrm{SV trk}>$"
    "/svtrksumptprofpt"
    <$= (view lvPt &&& svTrackSumPt)

zBTVsPt :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a) => Fill a
zBTVsPt =
  hist2DDef
    (binD 25 15 250)
    (binD 0 22 1.1)
    "$p_{\\mathrm T}$ [GeV]"
    "$z_{p_{\\mathrm T}}$"
    "/zbtvspt"
    <$= (view lvPt &&& zBT)

zBTProfPt :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a) => Fill a
zBTProfPt =
  prof1DDef
    (binD 25 15 250)
    "$p_{\\mathrm T}$ [GeV]"
    "$<z_{p_{\\mathrm T}}>$"
    "/zbtprofpt"
    <$= (view lvPt &&& zBT)

trkSumPtProfEta :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a) => Fill a
trkSumPtProfEta =
  prof1DDef
    (binD 0 21 2.1)
    "$\\eta$"
    "$<p_{\\mathrm T} \\sum \\mathrm{trk}>$"
    "/trksumptprofeta"
    <$= (view lvEta &&& trackSumPt)

svTrkSumPtProfEta :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a) => Fill a
svTrkSumPtProfEta =
  prof1DDef
    (binD 0 21 2.1)
    "$\\eta$"
    "$<p_{\\mathrm T} \\sum \\mathrm{SV trk}>$"
    "/svtrksumptprofeta"
    <$= (view lvEta &&& svTrackSumPt)

zBTProfEta :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a) => Fill a
zBTProfEta =
  prof1DDef
    (binD 0 21 2.1)
    "$\\eta$"
    "$<z_{p_{\\mathrm T}}>$"
    "/zbtprofeta"
    <$= (view lvEta &&& zBT)

svTrkSumPtProfTrkSumPt :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a) => Fill a
svTrkSumPtProfTrkSumPt =
  prof1DDef
    (binD 0 10 100)
    "$p_{\\mathrm T} \\sum \\mathrm{trk}$"
    "$<p_{\\mathrm T} \\sum \\mathrm{SV trk}>$"
    "/svtrksumptproftrksumpt"
    <$= (svTrackSumPt &&& trackSumPt)

zBTProfTrkSumPt :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a) => Fill a
zBTProfTrkSumPt =
  prof1DDef
    (binD 0 10 100)
    "$p_{\\mathrm T} \\sum \\mathrm{trk}$"
    "$<z_{p_{\\mathrm T}}>$"
    "/zbtproftrksumpt"
    <$= (trackSumPt &&& zBT)

nPVTrksH :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a) => Fill a
nPVTrksH =
  hist1DDef
    (binD 0 20 20)
    "$n$ PV tracks"
    (dsigdXpbY "n" "1")
    "/npvtrks"
    <$= fromIntegral . length . pvTracks

nSVTrksH :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a) => Fill a
nSVTrksH =
  hist1DDef
    (binD 0 10 10)
    "$n$ SV tracks"
    (dsigdXpbY "n" "1")
    "/nsvtrks"
    <$= fromIntegral . length . svTracks

nPVTrksProfPt :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a) => Fill a
nPVTrksProfPt =
  prof1DDef
    (binD 25 18 250)
    "$p_{\\mathrm T}$ [GeV]"
    "$<n$ PV tracks $>$"
    "/npvtrksprofpt"
    <$= (view lvPt &&& (fromIntegral . length . pvTracks))

nSVTrksProfPt :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a) => Fill a
nSVTrksProfPt =
  prof1DDef
    (binD 25 18 250)
    "$p_{\\mathrm T}$ [GeV]"
    "$<n$ SV tracks $>$"
    "/nsvtrksprofpt"
    <$= (view lvPt &&& (fromIntegral . length . svTracks))

bfragHs :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a) => Fill a
bfragHs = mconcat
  [ trkSumPtH
  , svTrkSumPtH
  , zBTH
  , trkSumPtProfPt
  , svTrkSumPtProfPt
  , zBTProfPt
  , trkSumPtProfEta
  , svTrkSumPtProfEta
  , zBTProfEta
  , nPVTrksH
  , nSVTrksH
  , nPVTrksProfPt
  , nSVTrksProfPt
  , zBTVsPt
  ]
