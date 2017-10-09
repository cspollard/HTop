{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module BFrag.BFrag where

import           Atlas
import           Control.Applicative (Alternative (..))
import           Control.Arrow       ((&&&))
import           Control.Lens
import           Data.Foldable       (fold)
import           Data.Vector         (Vector, (!))
import qualified Data.Vector         as V
import           GHC.Exts


class HasSVConstits a where
  svConstits :: a -> PhysObj [PtEtaPhiE]
  svChargedConstits :: a -> PhysObj [PtEtaPhiE]

class HasPVConstits a where
  pvConstits :: a -> PhysObj [PtEtaPhiE]
  pvChargedConstits :: a -> PhysObj [PtEtaPhiE]

constitsSum :: (HasSVConstits a, HasPVConstits a) => a -> PhysObj PtEtaPhiE
constitsSum j = do
  svt <- svConstits j
  pvt <- pvConstits j
  return . fold $ svt ++ pvt

chargedSum :: (HasSVConstits a, HasPVConstits a) => a -> PhysObj PtEtaPhiE
chargedSum j = do
  svt <- svChargedConstits j
  pvt <- pvChargedConstits j
  return . fold $ svt ++ pvt


zbtc, zblc
  :: (HasLorentzVector a, HasSVConstits a, HasPVConstits a)
  => a -> PhysObj Double
zbtc j = do
  svp4 <- fold <$> svChargedConstits j
  p4 <- chargedSum j
  case view lvPt p4 of
    0.0 -> empty
    x   -> pure $ view lvPt svp4 / x

zblc = undefined

zbt, zbl
  :: (HasLorentzVector a, HasSVConstits a, HasPVConstits a)
  => a -> PhysObj Double
zbt j = do
  svp4 <- fold <$> svConstits j
  p4 <- constitsSum j
  case view lvPt p4 of
    0.0 -> empty
    x   -> return $ view lvPt svp4 / x

zbl = undefined


chargedPtH
  :: (HasLorentzVector a, HasSVConstits a, HasPVConstits a)
  => Foldl (PhysObj a) (Vars YodaObj)
chargedPtH = physObjH h =$<< fmap (view lvPt) . chargedSum

  where
    h =
      hist1DDef
        (binD 0 25 250)
        "charged $p_{\\mathrm T}$"
        (dndx pt gev)


pvPtH
  :: (HasLorentzVector a, HasPVConstits a)
  => Foldl (PhysObj a) (Vars YodaObj)
pvPtH = physObjH h =$<< fmap (view lvPt . fold) . pvConstits

  where
    h =
      hist1DDef
        (binD 0 25 250)
        "PV $p_{\\mathrm T}$"
        (dndx pt gev)

pvPtcH
  :: (HasLorentzVector a, HasPVConstits a)
  => Foldl (PhysObj a) (Vars YodaObj)
pvPtcH = physObjH h =$<< fmap (view lvPt . fold) . pvChargedConstits

  where
    h =
      hist1DDef
        (binD 0 25 250)
        "PV charged $p_{\\mathrm T}$"
        (dndx pt gev)

svPtH
  :: (HasLorentzVector a, HasSVConstits a)
  => Foldl (PhysObj a) (Vars YodaObj)
svPtH = physObjH h =$<< fmap (view lvPt . fold) . svConstits

  where
    h =
      hist1DDef
        (binD 0 25 250)
        "SV $p_{\\mathrm T}$"
        (dndx pt gev)

svPtcH
  :: (HasLorentzVector a, HasSVConstits a)
  => Foldl (PhysObj a) (Vars YodaObj)
svPtcH = physObjH h =$<< fmap (view lvPt . fold) . svChargedConstits

  where
    h =
      hist1DDef
        (binD 0 25 250)
        "SV charged $p_{\\mathrm T}$"
        (dndx pt gev)


zbtcH
  :: (HasLorentzVector a, HasSVConstits a, HasPVConstits a)
  => Foldl (PhysObj a) (Vars YodaObj)
zbtcH = physObjH h =$<< zbtc

  where
    h =
      hist1DDef
        (binD 0 21 1.05)
        "charged $z_{p_{\\mathrm T}}$"
        (dndx "z_{p_{\\mathrm T}}" "1")

zbtH
  :: (HasLorentzVector a, HasSVConstits a, HasPVConstits a)
  => Foldl (PhysObj a) (Vars YodaObj)
zbtH = physObjH h =$<< zbt

  where
    h =
      hist1DDef
        (binD 0 21 1.05)
        "$z_{p_{\\mathrm T}}$"
        (dndx "z_{p_{\\mathrm T}}" "1")


nPVTracksH
  :: (HasLorentzVector a, HasPVConstits a)
  => Foldl (PhysObj a) (Vars YodaObj)
nPVTracksH = physObjH h =$<< fmap (fromIntegral . length) . pvChargedConstits

  where
    h =
      hist1DDef
        (binD 0 20 20)
        "$n$ PV tracks"
        (dndx "n" "1")


nSVTracksH
  :: (HasLorentzVector a, HasSVConstits a)
  => Foldl (PhysObj a) (Vars YodaObj)
nSVTracksH =
  physObjH h =$<< fmap (fromIntegral . length) . svChargedConstits

  where
    h =
      hist1DDef
        (binD 0 20 20)
        "$n$ SV tracks"
        (dndx "n" "1")


bfragHs
  :: (HasPVConstits a, HasSVConstits a, HasLorentzVector a)
  => Fills a
bfragHs =
  mconcat
  [ singleton "/zbt" <$> zbtH
  , singleton "/zbtc" <$> zbtcH
  , singleton "/chargedpt" <$> chargedPtH
  , singleton "/pvpt" <$> pvPtH
  , singleton "/pvptc" <$> pvPtcH
  , singleton "/svpt" <$> svPtH
  , singleton "/svptc" <$> svPtcH
  , singleton "/npvtracks" <$> nPVTracksH
  , singleton "/nsvtracks" <$> nSVTracksH

--     -- , childSumPtProfPt
--     -- , svChildSumPtProfPt
--     -- , zbtChargedProfPt
--     -- , childSumPtProfEta
--     -- , svChildSumPtProfEta
--     -- , zbtChargedProfEta
--     -- , nPVChildProfPt
--     -- , nSVChildProfPt
--     -- , zbtChargedVsPt
  ]

recoMerges :: [[Int]]
recoMerges =
  [ [00, 01, 02, 03, 04, 05, 06]
  , [07, 08, 09]
  , [10, 11]
  , [12, 13]
  , [14]
  , [15]
  , [16]
  , [17]
  , [18]
  , [19]
  , [20]
  ]

trueMerges :: [[Int]]
trueMerges =
  [ [00, 01, 02, 03, 04, 05, 06]
  , [07, 08, 09]
  , [10, 11]
  , [12, 13]
  , [14]
  , [15]
  , [16]
  , [17]
  , [18]
  , [19]
  , [20]
  ]

trimV :: Monoid a => [[Int]] -> Vector a -> Vector a
trimV = mergeV mappend mempty

trimB :: [[Int]] -> ArbBin a -> ArbBin a
trimB bm bs = foldl (flip $ uncurry mergeBinRange) bs . reverse $ (head &&& last) <$> bm

trimH
  :: (Monoid b, Ord a, Fractional a)
  => [[Int]]
  -> Histogram Vector (ArbBin a) b
  -> Histogram Vector (ArbBin a) b
trimH bm h =
  let v = views histData (trimV bm) h
      b = views bins (trimB bm) h
  in histogramUO b Nothing v


trimTrueH, trimRecoH
  :: (Monoid b, Ord a, Fractional a)
  => Histogram Vector (ArbBin a) b
  -> Histogram Vector (ArbBin a) b
trimTrueH = trimH trueMerges
trimRecoH = trimH recoMerges


mergeV :: (a -> a -> a) -> a -> [[Int]] -> Vector a -> Vector a
mergeV f x ks v = V.fromList $ go <$> ks
  where
    go is = foldl f x $ (v !) <$> is


matrixname, recohname, recomatchhname, truehname :: IsString s => s
matrixname = "/elmujjmatched/zbtcmig"
recohname = "/elmujj/probejets/zbtc"
recomatchhname = "/elmujjmatched/probejets/zbtc"
truehname = "/elmujjtrue/truejets/zbtc"


-- childSumPtProfPt
--   :: (HasLorentzVector a, HasSVConstits a, HasPVConstits a)
--   => Foldl (a, Double) YodaObj
-- childSumPtProfPt =
--   fmap (singleton "/childsumptprofpt")
--   $ prof1DDef
--     (binD 25 18 250)
--     "$p_{\\mathrm T}$ [GeV]"
--     "$<p_{\\mathrm T} \\sum \\mathrm{child}>$"
--     <$= first (view lvPt &&& trackSumPt)
--
--
-- svChildSumPtProfPt
--   :: (HasLorentzVector a, HasSVConstits a, HasPVConstits a)
--   => Foldl (a, Double) YodaObj
-- svChildSumPtProfPt =
--   fmap (singleton "/svchildsumptprofpt")
--   $  prof1DDef
--     (binD 25 18 250)
--     "$p_{\\mathrm T}$ [GeV]"
--     "$<p_{\\mathrm T} \\sum \\mathrm{SV child}>$"
--     <$= first (view lvPt &&& svConstitsSumPt)
--
--
-- zbtChargedVsPt
--   :: (HasLorentzVector a, HasSVConstits a, HasPVConstits a)
--   => Foldl (a, Double) YodaObj
-- zbtChargedVsPt =
--   fmap (singleton "/zbtvspt")
--   $ hist2DDef
--     (binD 25 18 250)
--     (binD 0 21 1.05)
--     "$p_{\\mathrm T}$ [GeV]"
--     "$z_{p_{\\mathrm T}}$"
--     <$= first (view lvPt &&& zbtCharged)
--
--
-- zbtChargedProfPt
--   :: (HasLorentzVector a, HasSVConstits a, HasPVConstits a)
--   => Foldl (a, Double) YodaObj
-- zbtChargedProfPt =
--   fmap (singleton "/zbtprofpt")
--   $ prof1DDef
--     (binD 25 18 250)
--     "$p_{\\mathrm T}$ [GeV]"
--     "$<z_{p_{\\mathrm T}}>$"
--     <$= first (view lvPt &&& zbtCharged)
--
--
-- childSumPtProfEta
--   :: (HasLorentzVector a, HasSVConstits a, HasPVConstits a)
--   => Foldl (a, Double) YodaObj
-- childSumPtProfEta =
--   fmap (singleton "/childsumptprofeta")
--   $ prof1DDef
--     (binD 0 21 2.1)
--     "$\\eta$"
--     "$<p_{\\mathrm T} \\sum \\mathrm{child}>$"
--     <$= first (view lvEta &&& trackSumPt)
--
-- svChildSumPtProfEta
--   :: (HasLorentzVector a, HasSVConstits a, HasPVConstits a)
--   => Foldl (a, Double) YodaObj
-- svChildSumPtProfEta =
--   fmap (singleton "/svchildsumptprofeta")
--   $ prof1DDef
--     (binD 0 21 2.1)
--     "$\\eta$"
--     "$<p_{\\mathrm T} \\sum \\mathrm{SV child}>$"
--     <$= first (view lvEta &&& svConstitsSumPt)
--
--
-- zbtChargedProfEta
--   :: (HasLorentzVector a, HasSVConstits a, HasPVConstits a)
--   => Foldl (a, Double) YodaObj
-- zbtChargedProfEta =
--   fmap (singleton "/zbtprofeta")
--   $ prof1DDef
--     (binD 0 21 2.1)
--     "$\\eta$"
--     "$<z_{p_{\\mathrm T}}>$"
--     <$= first (view lvEta &&& zbtCharged)
--
--
-- svChildSumPtProfChildSumPt
--   :: (HasLorentzVector a, HasSVConstits a, HasPVConstits a)
--   => Foldl (a, Double) YodaObj
-- svChildSumPtProfChildSumPt =
--   fmap (singleton "/svchildsumptprofchildsumpt")
--   $ prof1DDef
--     (binD 0 25 100)
--     "$p_{\\mathrm T} \\sum \\mathrm{child}$"
--     "$<p_{\\mathrm T} \\sum \\mathrm{SV child}>$"
--     <$= first (svConstitsSumPt &&& trackSumPt)
--
--
-- zbtChargedProfChildSumPt
--   :: (HasLorentzVector a, HasSVConstits a, HasPVConstits a)
--   => Foldl (a, Double) YodaObj
-- zbtChargedProfChildSumPt =
--   fmap (singleton "/zbtprofchildsumpt")
--   $ prof1DDef
--     (binD 0 25 100)
--     "$p_{\\mathrm T} \\sum \\mathrm{child}$"
--     "$<z_{p_{\\mathrm T}}>$"
--     <$= first (trackSumPt &&& zbtCharged)
--
--
--
-- nPVChildProfPt
--   :: (HasLorentzVector a, HasSVConstits a, HasPVConstits a)
--   => Foldl (a, Double) YodaObj
-- nPVChildProfPt =
--   fmap (singleton "/npvchildsprofpt")
--   $ prof1DDef
--     (binD 25 18 250)
--     "$p_{\\mathrm T}$ [GeV]"
--     "$<n$ PV tracks $>$"
--     <$= first (view lvPt &&& (fromIntegral . nPVConstits))
--
--
-- nSVChildProfPt
--   :: (HasLorentzVector a, HasSVConstits a, HasPVConstits a)
--   => Foldl (a, Double) YodaObj
-- nSVChildProfPt =
--   fmap (singleton "/nsvchildsprofpt")
--   $ prof1DDef
--     (binD 25 18 250)
--     "$p_{\\mathrm T}$ [GeV]"
--     "$<n$ SV tracks $>$"
--     <$= first (view lvPt &&& (fromIntegral . nSVConstits))
