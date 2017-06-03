{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module BFrag.BFrag where

import           Atlas
import           Control.Arrow  ((&&&))
import           Control.Lens
import           Data.Bifunctor
import           Data.Foldable  (fold)
import           Data.Semigroup

class HasSVTracks a where
  svTracks :: a -> [PtEtaPhiE]

svTrackSum :: HasSVTracks a => a -> PtEtaPhiE
svTrackSum = fold . svTracks

svTrackSumPt :: HasSVTracks a => a -> Double
svTrackSumPt = view lvPt . svTrackSum

nSVTracks :: HasSVTracks a => a -> Int
nSVTracks = length . svTracks


class HasPVTracks a where
  pvTracks :: a -> [PtEtaPhiE]

pvTrackSum :: HasPVTracks a => a -> PtEtaPhiE
pvTrackSum = fold . pvTracks

pvTrackSumPt :: HasPVTracks a => a -> Double
pvTrackSumPt = view lvPt . pvTrackSum

nPVTracks :: HasPVTracks a => a -> Int
nPVTracks = length . pvTracks

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
  fmap (singleton "/trksumpt") . physObjH
  $ hist1DDef
    (binD 0 25 250)
    "$p_{\\mathrm T} \\sum \\mathrm{trk}$"
    (dndx pt gev)
    <$= first trackSumPt


svTrkSumPtH
  :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a)
  => Fills a
svTrkSumPtH =
  fmap (singleton "/svtrksumpt") . physObjH
  $ hist1DDef
    (binD 0 25 250)
    "$p_{\\mathrm T} \\sum \\mathrm{SV trk}$"
    (dndx pt gev)
    <$= first svTrackSumPt


zBTH
  :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a)
  => Int -> Fills a
zBTH nbins =
  fmap (singleton "/zbt") . physObjH
  $ hist1DDef
    (binD 0 nbins 1.05)
    "$z_{p_{\\mathrm T}}$"
    (dndx "z_{p_{\\mathrm T}}" "1")
    <$= first zBT


trkSumPtProfPt
  :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a)
  => Fills a
trkSumPtProfPt =
  fmap (singleton "/trksumptprofpt") . physObjH
  $ prof1DDef
    (binD 25 18 250)
    "$p_{\\mathrm T}$ [GeV]"
    "$<p_{\\mathrm T} \\sum \\mathrm{trk}>$"
    <$= first (view lvPt &&& trackSumPt)


svTrkSumPtProfPt
  :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a)
  => Fills a
svTrkSumPtProfPt =
  fmap (singleton "/svtrksumptprofpt") . physObjH
  $  prof1DDef
    (binD 25 18 250)
    "$p_{\\mathrm T}$ [GeV]"
    "$<p_{\\mathrm T} \\sum \\mathrm{SV trk}>$"
    <$= first (view lvPt &&& svTrackSumPt)


zBTVsPt
  :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a)
  => Fills a
zBTVsPt =
  fmap (singleton "/zbtvspt") . physObjH
  $ hist2DDef
    (binD 25 18 250)
    (binD 0 21 1.05)
    "$p_{\\mathrm T}$ [GeV]"
    "$z_{p_{\\mathrm T}}$"
    <$= first (view lvPt &&& zBT)


zBTProfPt
  :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a)
  => Fills a
zBTProfPt =
  fmap (singleton "/zbtprofpt") . physObjH
  $ prof1DDef
    (binD 25 18 250)
    "$p_{\\mathrm T}$ [GeV]"
    "$<z_{p_{\\mathrm T}}>$"
    <$= first (view lvPt &&& zBT)


trkSumPtProfEta
  :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a)
  => Fills a
trkSumPtProfEta =
  fmap (singleton "/trksumptprofeta") . physObjH
  $ prof1DDef
    (binD 0 21 2.1)
    "$\\eta$"
    "$<p_{\\mathrm T} \\sum \\mathrm{trk}>$"
    <$= first (view lvEta &&& trackSumPt)

svTrkSumPtProfEta
  :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a)
  => Fills a
svTrkSumPtProfEta =
  fmap (singleton "/svtrksumptprofeta") . physObjH
  $ prof1DDef
    (binD 0 21 2.1)
    "$\\eta$"
    "$<p_{\\mathrm T} \\sum \\mathrm{SV trk}>$"
    <$= first (view lvEta &&& svTrackSumPt)


zBTProfEta
  :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a)
  => Fills a
zBTProfEta =
  fmap (singleton "/zbtprofeta") . physObjH
  $ prof1DDef
    (binD 0 21 2.1)
    "$\\eta$"
    "$<z_{p_{\\mathrm T}}>$"
    <$= first (view lvEta &&& zBT)


svTrkSumPtProfTrkSumPt
  :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a)
  => Fills a
svTrkSumPtProfTrkSumPt =
  fmap (singleton "/svtrksumptproftrksumpt") . physObjH
  $ prof1DDef
    (binD 0 25 100)
    "$p_{\\mathrm T} \\sum \\mathrm{trk}$"
    "$<p_{\\mathrm T} \\sum \\mathrm{SV trk}>$"
    <$= first (svTrackSumPt &&& trackSumPt)


zBTProfTrkSumPt
  :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a)
  => Fills a
zBTProfTrkSumPt =
  fmap (singleton "/zbtproftrksumpt") . physObjH
  $ prof1DDef
    (binD 0 25 100)
    "$p_{\\mathrm T} \\sum \\mathrm{trk}$"
    "$<z_{p_{\\mathrm T}}>$"
    <$= first (trackSumPt &&& zBT)

nPVTrksH
  :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a)
  => Fills a
nPVTrksH =
  fmap (singleton "/npvtrks") . physObjH
  $ hist1DDef
    (binD 0 20 20)
    "$n$ PV tracks"
    (dndx "n" "1")
    <$= first (fromIntegral . length . pvTracks)


nSVTrksH
  :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a)
  => Fills a
nSVTrksH =
  fmap (singleton "/nsvtrks") . physObjH
  $ hist1DDef
    (binD 0 10 10)
    "$n$ SV tracks"
    (dndx "n" "1")
    <$= first (fromIntegral . nSVTracks)


nPVTrksProfPt
  :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a)
  => Fills a
nPVTrksProfPt =
  fmap (singleton "/npvtrksprofpt") . physObjH
  $ prof1DDef
    (binD 25 18 250)
    "$p_{\\mathrm T}$ [GeV]"
    "$<n$ PV tracks $>$"
    <$= first (view lvPt &&& (fromIntegral . nPVTracks))


nSVTrksProfPt
  :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a)
  => Fills a
nSVTrksProfPt =
  fmap (singleton "/nsvtrksprofpt") . physObjH
  $ prof1DDef
    (binD 25 18 250)
    "$p_{\\mathrm T}$ [GeV]"
    "$<n$ SV tracks $>$"
    <$= first (view lvPt &&& (fromIntegral . nSVTracks))


bfragHs
  :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a)
  => Int -> Fills a
bfragHs nbins =
  mconcat
    [ zBTH nbins
    , trkSumPtH
    , svTrkSumPtH
    -- , trkSumPtProfPt
    -- , svTrkSumPtProfPt
    -- , zBTProfPt
    -- , trkSumPtProfEta
    -- , svTrkSumPtProfEta
    -- , zBTProfEta
    , nPVTrksH
    , nSVTrksH
    -- , nPVTrksProfPt
    -- , nSVTrksProfPt
    -- , zBTVsPt
    ]
