{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module BFrag.BFrag where

import           Atlas
import           Control.Arrow  ((&&&))
import           Control.Lens
import           Data.Foldable  (fold)
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

trkSumPtH
  :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a)
  => Fills a
trkSumPtH =
  singleton "/trksumpt"
  <$> hist1DDef
    (binD 0 25 500)
    "$p_{\\mathrm T} \\sum \\mathrm{trk}$"
    (dsigdXpbY pt gev)
  <$= trackSumPt

svTrkSumPtH
  :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a)
  => Fills a
svTrkSumPtH =
  singleton "/svtrksumpt"
  <$> hist1DDef
    (binD 0 25 500)
    "$p_{\\mathrm T} \\sum \\mathrm{SV trk}$"
    (dsigdXpbY pt gev)
  <$= svTrackSumPt

zBTH
  :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a)
  => Fills a
zBTH =
  singleton "/zbt"
  <$> hist1DDef
    (binD 0 22 1.1)
    "$z_{p_{\\mathrm T}}$"
    (dsigdXpbY "z_{p_{\\mathrm T}}" "1")
  <$= zBT

trkSumPtProfPt
  :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a)
  => Fills a
trkSumPtProfPt =
  singleton "/trksumptprofpt"
  <$> prof1DDef
    (binD 25 18 250)
    "$p_{\\mathrm T}$ [GeV]"
    "$<p_{\\mathrm T} \\sum \\mathrm{trk}>$"
  <$= (view lvPt &&& trackSumPt)

svTrkSumPtProfPt
  :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a)
  => Fills a
svTrkSumPtProfPt =
  singleton "/svtrksumptprofpt"
  <$> prof1DDef
    (binD 25 18 250)
    "$p_{\\mathrm T}$ [GeV]"
    "$<p_{\\mathrm T} \\sum \\mathrm{SV trk}>$"
  <$= (view lvPt &&& svTrackSumPt)

zBTVsPt
  :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a)
  => Fills a
zBTVsPt =
  singleton "/zbtvspt"
  <$> hist2DDef
    (binD 25 15 250)
    (binD 0 22 1.1)
    "$p_{\\mathrm T}$ [GeV]"
    "$z_{p_{\\mathrm T}}$"
  <$= (view lvPt &&& zBT)

zBTProfPt
  :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a)
  => Fills a
zBTProfPt =
  singleton "/zbtprofpt"
  <$> prof1DDef
    (binD 25 15 250)
    "$p_{\\mathrm T}$ [GeV]"
    "$<z_{p_{\\mathrm T}}>$"
  <$= (view lvPt &&& zBT)

trkSumPtProfEta
  :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a)
  => Fills a
trkSumPtProfEta =
  singleton "/trksumptprofeta"
  <$> prof1DDef
    (binD 0 21 2.1)
    "$\\eta$"
    "$<p_{\\mathrm T} \\sum \\mathrm{trk}>$"
  <$= (view lvEta &&& trackSumPt)

svTrkSumPtProfEta
  :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a)
  => Fills a
svTrkSumPtProfEta =
  singleton "/svtrksumptprofeta"
  <$> prof1DDef
    (binD 0 21 2.1)
    "$\\eta$"
    "$<p_{\\mathrm T} \\sum \\mathrm{SV trk}>$"
  <$= (view lvEta &&& svTrackSumPt)

zBTProfEta
  :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a)
  => Fills a
zBTProfEta =
  singleton "/zbtprofeta"
  <$> prof1DDef
    (binD 0 21 2.1)
    "$\\eta$"
    "$<z_{p_{\\mathrm T}}>$"
  <$= (view lvEta &&& zBT)

svTrkSumPtProfTrkSumPt
  :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a)
  => Fills a
svTrkSumPtProfTrkSumPt =
  singleton "/svtrksumptproftrksumpt"
  <$> prof1DDef
    (binD 0 10 100)
    "$p_{\\mathrm T} \\sum \\mathrm{trk}$"
    "$<p_{\\mathrm T} \\sum \\mathrm{SV trk}>$"
  <$= (svTrackSumPt &&& trackSumPt)

zBTProfTrkSumPt
  :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a)
  => Fills a
zBTProfTrkSumPt =
  singleton "/zbtproftrksumpt"
  <$> prof1DDef
    (binD 0 10 100)
    "$p_{\\mathrm T} \\sum \\mathrm{trk}$"
    "$<z_{p_{\\mathrm T}}>$"
  <$= (trackSumPt &&& zBT)

nPVTrksH
  :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a)
  => Fills a
nPVTrksH =
  singleton "/npvtrks"
  <$> hist1DDef
    (binD 0 20 20)
    "$n$ PV tracks"
    (dsigdXpbY "n" "1")
  <$= fromIntegral . length . pvTracks

nSVTrksH
  :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a)
  => Fills a
nSVTrksH =
  singleton "/nsvtrks"
  <$> hist1DDef
    (binD 0 10 10)
    "$n$ SV tracks"
    (dsigdXpbY "n" "1")
  <$= fromIntegral . length . svTracks

nPVTrksProfPt
  :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a)
  => Fills a
nPVTrksProfPt =
  singleton "/npvtrksprofpt"
  <$> prof1DDef
    (binD 25 18 250)
    "$p_{\\mathrm T}$ [GeV]"
    "$<n$ PV tracks $>$"
  <$= (view lvPt &&& (fromIntegral . length . pvTracks))

nSVTrksProfPt
  :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a)
  => Fills a
nSVTrksProfPt =
  singleton "/nsvtrksprofpt"
  <$> prof1DDef
    (binD 25 18 250)
    "$p_{\\mathrm T}$ [GeV]"
    "$<n$ SV tracks $>$"
  <$= (view lvPt &&& (fromIntegral . length . svTracks))

bfragHs
  :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a)
  => Fills a
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
