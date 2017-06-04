{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module BFrag.BFrag where

import           Atlas
import           Control.Lens
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

-- trkSumPtH
--   :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a)
--   => Foldl (a, Double) YodaObj
-- trkSumPtH =
--   fmap (singleton "/trksumpt")
--   $ hist1DDef
--     (binD 0 25 250)
--     "$p_{\\mathrm T} \\sum \\mathrm{trk}$"
--     (dndx pt gev)
--     <$= first trackSumPt
--
--
-- svTrkSumPtH
--   :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a)
--   => Foldl (a, Double) YodaObj
-- svTrkSumPtH =
--   fmap (singleton "/svtrksumpt")
--   $ hist1DDef
--     (binD 0 25 250)
--     "$p_{\\mathrm T} \\sum \\mathrm{SV trk}$"
--     (dndx pt gev)
--     <$= first svTrackSumPt
--
--
-- zBTH
--   :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a)
--   => Int -> Foldl (a, Double) YodaObj
-- zBTH nbins =
--   fmap (singleton "/zbt")
--   $ hist1DDef
--     (binD 0 nbins 1.05)
--     "$z_{p_{\\mathrm T}}$"
--     (dndx "z_{p_{\\mathrm T}}" "1")
--     <$= first zBT
--
--
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
-- zBTVsPt
--   :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a)
--   => Foldl (a, Double) YodaObj
-- zBTVsPt =
--   fmap (singleton "/zbtvspt")
--   $ hist2DDef
--     (binD 25 18 250)
--     (binD 0 21 1.05)
--     "$p_{\\mathrm T}$ [GeV]"
--     "$z_{p_{\\mathrm T}}$"
--     <$= first (view lvPt &&& zBT)
--
--
-- zBTProfPt
--   :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a)
--   => Foldl (a, Double) YodaObj
-- zBTProfPt =
--   fmap (singleton "/zbtprofpt")
--   $ prof1DDef
--     (binD 25 18 250)
--     "$p_{\\mathrm T}$ [GeV]"
--     "$<z_{p_{\\mathrm T}}>$"
--     <$= first (view lvPt &&& zBT)
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
-- zBTProfEta
--   :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a)
--   => Foldl (a, Double) YodaObj
-- zBTProfEta =
--   fmap (singleton "/zbtprofeta")
--   $ prof1DDef
--     (binD 0 21 2.1)
--     "$\\eta$"
--     "$<z_{p_{\\mathrm T}}>$"
--     <$= first (view lvEta &&& zBT)
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
-- zBTProfTrkSumPt
--   :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a)
--   => Foldl (a, Double) YodaObj
-- zBTProfTrkSumPt =
--   fmap (singleton "/zbtproftrksumpt")
--   $ prof1DDef
--     (binD 0 25 100)
--     "$p_{\\mathrm T} \\sum \\mathrm{trk}$"
--     "$<z_{p_{\\mathrm T}}>$"
--     <$= first (trackSumPt &&& zBT)
--
-- nPVTrksH
--   :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a)
--   => Foldl (a, Double) YodaObj
-- nPVTrksH =
--   fmap (singleton "/npvtrks")
--   $ hist1DDef
--     (binD 0 20 20)
--     "$n$ PV tracks"
--     (dndx "n" "1")
--     <$= first (fromIntegral . length . pvTracks)
--
--
-- nSVTrksH
--   :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a)
--   => Foldl (a, Double) YodaObj
-- nSVTrksH =
--   fmap (singleton "/nsvtrks")
--   $ hist1DDef
--     (binD 0 10 10)
--     "$n$ SV tracks"
--     (dndx "n" "1")
--     <$= first (fromIntegral . nSVTracks)
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

bfragHs
  :: (HasLorentzVector a, HasSVTracks a, HasPVTracks a)
  => Int -> Foldl (a, Double) (Folder YodaObj)
bfragHs nbins = mempty

--   mconcat
--     [ zBTH nbins
--     , trkSumPtH
--     , svTrkSumPtH
--     -- , trkSumPtProfPt
--     -- , svTrkSumPtProfPt
--     -- , zBTProfPt
--     -- , trkSumPtProfEta
--     -- , svTrkSumPtProfEta
--     -- , zBTProfEta
--     , nPVTrksH
--     , nSVTrksH
--     -- , nPVTrksProfPt
--     -- , nSVTrksProfPt
--     -- , zBTVsPt
--     ]
