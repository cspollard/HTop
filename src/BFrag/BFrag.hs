{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module BFrag.BFrag where

import           Atlas
import           Control.Applicative    (Alternative (..))
import           Control.Arrow          ((&&&))
import           Control.Lens
import           Data.Foldable          (fold)
import           Data.HEP.ThreeMomentum
import           Data.Text              (Text)
import           Data.Vector            (Vector, (!))
import qualified Data.Vector            as V
import           GHC.Exts

zbtname, zblname, zbtrelname :: Text
zbtname = "\\ensuremath{z_{\\mathrm{T,B}}}"
zblname = "\\ensuremath{z_{\\mathrm{L,B}}}"
zbtrelname = "\\ensuremath{z_{\\mathrm{T,B}}^\\mathrm{rel}}"

zbtcname, zblcname, zbtrelcname :: Text
zbtcname = "\\ensuremath{z_{\\mathrm{T,B}}^\\mathrm{ch}}"
zblcname = "\\ensuremath{z_{\\mathrm{L,B}}^\\mathrm{ch}}"
zbtrelcname = "\\ensuremath{z_{\\mathrm{T,B}}^\\mathrm{ch, rel}}"

zbtbin, zbtcbin, zblbin, zblcbin, zbtrelbin, zbtrelcbin :: BinD
zbtbin = binD 0 21 1.05
zbtcbin = zbtbin
zblbin = zbtbin
zblcbin = zblbin
zbtrelbin = binD 0 20 0.05
zbtrelcbin = zbtrelbin

npvtrkname, nsvtrkname :: Text
npvtrkname = "\\ensuremath{n_{\\mathrm{PV}}^\\mathrm{ch}}"
nsvtrkname = "\\ensuremath{n_{\\mathrm{B}}^\\mathrm{ch}}"

npvtrkbin, nsvtrkbin :: BinD
npvtrkbin = binD 0 20 20
nsvtrkbin = binD 0 20 20

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


zbtc, zblc, zbtrelc
  :: (HasSVConstits a, HasPVConstits a)
  => a -> PhysObj Double
zbtc j = do
  svp4 <- fold <$> svChargedConstits j
  p4 <- chargedSum j
  case view lvPt p4 of
    0.0 -> empty
    x   -> pure $ view lvPt svp4 / x

zblc j = do
  svp3 <- toXYZ . fold <$> svChargedConstits j
  p3 <- toXYZ <$> chargedSum j
  let denom = modulus2 p3
      num = svp3 `inner` p3
  return $ num / denom

zbtrelc j = do
  svp3 <- toXYZ . fold <$> svChargedConstits j
  p3 <- toXYZ <$> chargedSum j
  let denom = modulus2 p3
      num = modulus $ svp3 `cross` p3
  return $ num / denom

zbt, zbl, zbtrel
  :: (HasSVConstits a, HasPVConstits a)
  => a -> PhysObj Double
zbt j = do
  svp4 <- fold <$> svConstits j
  p4 <- constitsSum j
  case view lvPt p4 of
    0.0 -> empty
    x   -> return $ view lvPt svp4 / x

zbl j = do
  svp3 <- toXYZ . fold <$> svConstits j
  p3 <- toXYZ <$> constitsSum j
  let denom = modulus2 p3
      num = svp3 `inner` p3
  return $ num / denom

zbtrel j = do
  svp3 <- toXYZ . fold <$> svConstits j
  p3 <- toXYZ <$> constitsSum j
  let denom = modulus2 p3
      num = modulus $ svp3 `cross` p3
  return $ num / denom

chargedPtH
  :: (HasSVConstits a, HasPVConstits a)
  => Foldl (PhysObj a) (Vars YodaObj)
chargedPtH = physObjH h =$<< fmap (view lvPt) . chargedSum

  where
    h =
      hist1DDef
        (binD 0 25 250)
        "charged $p_{\\mathrm T}$"
        (dsigdXpbY pt gev)


pvPtH
  :: (HasPVConstits a)
  => Foldl (PhysObj a) (Vars YodaObj)
pvPtH = physObjH h =$<< fmap (view lvPt . fold) . pvConstits

  where
    h =
      hist1DDef
        (binD 0 25 250)
        "PV $p_{\\mathrm T}$"
        (dsigdXpbY pt gev)

pvPtcH
  :: (HasPVConstits a)
  => Foldl (PhysObj a) (Vars YodaObj)
pvPtcH = physObjH h =$<< fmap (view lvPt . fold) . pvChargedConstits

  where
    h =
      hist1DDef
        (binD 0 25 250)
        "PV charged $p_{\\mathrm T}$"
        (dsigdXpbY pt gev)

svPtH
  :: (HasSVConstits a)
  => Foldl (PhysObj a) (Vars YodaObj)
svPtH = physObjH h =$<< fmap (view lvPt . fold) . svConstits

  where
    h =
      hist1DDef
        (binD 0 25 250)
        "SV $p_{\\mathrm T}$"
        (dsigdXpbY pt gev)

svPtcH
  :: (HasSVConstits a)
  => Foldl (PhysObj a) (Vars YodaObj)
svPtcH = physObjH h =$<< fmap (view lvPt . fold) . svChargedConstits

  where
    h =
      hist1DDef
        (binD 0 25 250)
        "SV charged $p_{\\mathrm T}$"
        (dsigdXpbY pt gev)


zbtH :: (HasSVConstits a, HasPVConstits a) => Foldl (PhysObj a) (Vars YodaObj)
zbtH = physObjH h =$<< zbt

  where
    h = hist1DDef zbtbin zbtname (dsigdXpbY zbtname "1")

zbtcH :: (HasSVConstits a, HasPVConstits a) => Foldl (PhysObj a) (Vars YodaObj)
zbtcH = physObjH h =$<< zbtc
  where
    h = hist1DDef zbtcbin zbtcname (dsigdXpbY zbtcname "1")


zblH :: (HasSVConstits a, HasPVConstits a) => Foldl (PhysObj a) (Vars YodaObj)
zblH = physObjH h =$<< zbl
  where
    h = hist1DDef zblbin zblname (dsigdXpbY zblname "1")

zblcH :: (HasSVConstits a, HasPVConstits a) => Foldl (PhysObj a) (Vars YodaObj)
zblcH = physObjH h =$<< zblc
  where
    h = hist1DDef zblcbin zblcname (dsigdXpbY zblcname "1")


zbtrelH
  :: (HasSVConstits a, HasPVConstits a)
  => Foldl (PhysObj a) (Vars YodaObj)
zbtrelH = physObjH h =$<< zbtrel
  where
    h = hist1DDef zbtrelbin zbtrelname (dsigdXpbY zbtrelname "1")

zbtrelcH
  :: (HasSVConstits a, HasPVConstits a)
  => Foldl (PhysObj a) (Vars YodaObj)
zbtrelcH = physObjH h =$<< zbtrelc
  where
    h = hist1DDef zbtrelcbin zbtrelcname (dsigdXpbY zbtrelcname "1")



nPVTracksH
  :: (HasPVConstits a)
  => Foldl (PhysObj a) (Vars YodaObj)
nPVTracksH = physObjH h =$<< fmap (fromIntegral . length) . pvChargedConstits
  where
    h = hist1DDef npvtrkbin npvtrkname (dsigdXpbY npvtrkname "1")


nSVTracksH
  :: (HasLorentzVector a, HasSVConstits a)
  => Foldl (PhysObj a) (Vars YodaObj)
nSVTracksH =
  physObjH h =$<< fmap (fromIntegral . length) . svChargedConstits
  where
    h = hist1DDef nsvtrkbin nsvtrkname (dsigdXpbY nsvtrkname "1")


bfragHs
  :: (HasPVConstits a, HasSVConstits a, HasLorentzVector a)
  => Fills a
bfragHs =
  mconcat
  [ singleton "/zbt" <$> zbtH
  , singleton "/zbtc" <$> zbtcH
  , singleton "/zbl" <$> zblH
  , singleton "/zblc" <$> zblcH
  , singleton "/zbtrel" <$> zbtrelH
  , singleton "/zbtrelc" <$> zbtrelcH
  , singleton "/chargedpt" <$> chargedPtH
  , singleton "/pvpt" <$> pvPtH
  , singleton "/pvptc" <$> pvPtcH
  , singleton "/svpt" <$> svPtH
  , singleton "/svptc" <$> svPtcH
  , singleton "/npvtrk" <$> nPVTracksH
  , singleton "/nsvtrk" <$> nSVTracksH

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


zbtcMerges, zblcMerges, zbtrelcMerges :: [[Int]]
zbtcMerges =
  [ [00, 01, 02, 03, 04, 05]
  , [06, 07, 08, 09]
  , [10, 11, 12]
  , [13, 14]
  , [15, 16]
  , [17, 18]
  , [19, 20]
  ]

zblcMerges = zbtcMerges

zbtrelcMerges =
  [ [00, 01]
  , [02, 03]
  , [04, 05]
  , [06, 07]
  , [08, 09]
  , [10, 11, 12]
  , [11, 12, 13, 14]
  , [15, 16, 17, 18, 19]
  ]

obsTrimmers
  :: (Eq a1, Fractional a, Ord a, Monoid b, IsString a1)
  => a1 -> Histogram Vector (ArbBin a) b -> Histogram Vector (ArbBin a) b
obsTrimmers s =
  case s of
    "zbtc"    -> trimH zbtcMerges
    "zblc"    -> trimH zblcMerges
    "zbtrelc" -> trimH zbtrelcMerges
    _         -> id


obsNames
  :: (IsString t, IsString a, Eq a)
  => a -> (t, t, t, t)
obsNames s =
  case s of
    "zbtc"    -> (zbtcrecohname, zbtctruehname, zbtcrecomatchhname, zbtcmatrixname)
    "zblc"    -> (zblcrecohname, zblctruehname, zblcrecomatchhname, zblcmatrixname)
    "zbtrelc" -> (zbtrelcrecohname, zbtrelctruehname, zbtrelcrecomatchhname, zbtrelcmatrixname)
    _         -> error "unrecognized observable"


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

mergeV :: (a -> a -> a) -> a -> [[Int]] -> Vector a -> Vector a
mergeV f x ks v = V.fromList $ go <$> ks
  where
    go is = foldl f x $ (v !) <$> is


zbtcmatrixname, zbtcrecohname, zbtcrecomatchhname, zbtctruehname :: IsString s => s
zbtcmatrixname = "/elmujjmatched/zbtcmig"
zbtcrecohname = "/elmujj/probejets/zbtc"
zbtcrecomatchhname = "/elmujjmatched/probejets/zbtc"
zbtctruehname = "/elmujjtrue/truejets/zbtc"

zblcmatrixname, zblcrecohname, zblcrecomatchhname, zblctruehname :: IsString s => s
zblcmatrixname = "/elmujjmatched/zblcmig"
zblcrecohname = "/elmujj/probejets/zblc"
zblcrecomatchhname = "/elmujjmatched/probejets/zblc"
zblctruehname = "/elmujjtrue/truejets/zblc"

zbtrelcmatrixname, zbtrelcrecohname, zbtrelcrecomatchhname, zbtrelctruehname :: IsString s => s
zbtrelcmatrixname = "/elmujjmatched/zbtrelcmig"
zbtrelcrecohname = "/elmujj/probejets/zbtrelc"
zbtrelcrecomatchhname = "/elmujjmatched/probejets/zbtrelc"
zbtrelctruehname = "/elmujjtrue/truejets/zbtrelc"

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
