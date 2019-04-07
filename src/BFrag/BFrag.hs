{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module BFrag.BFrag where

import           Atlas
import           Control.Applicative    (Alternative (..))
import           Control.Arrow          ((&&&))
import           Control.Lens
import           Data.Foldable          (fold)
import           Data.HEP.ThreeMomentum
import           Data.Histogram.Generic (histogramUO)
import           Data.Text              (Text)
import           Data.Vector            (Vector, (!))
import qualified Data.Vector            as V
import           GHC.Exts


zbtname, zblname, zbrelname :: Text
zbtname = "\\ensuremath{z_{\\mathrm{T,b}}}"
zblname = "\\ensuremath{z_{\\mathrm{L,b}}}"
zbrelname = "\\ensuremath{z_{\\mathrm{rel,b}}}"


zbtcname, zblcname, zbrelcname :: Text
zbtcname = "\\ensuremath{z_{\\mathrm{T,b}}^\\mathrm{ch}}"
zblcname = "\\ensuremath{z_{\\mathrm{L,b}}^\\mathrm{ch}}"
zbrelcname = "\\ensuremath{z_{\\mathrm{rel,b}}^\\mathrm{ch}}"


zbtbin, zbtcbin, zblbin, zblcbin, zbrelbin, zbrelcbin :: BinD
zbtbin = binD 0 21 1.05
zbtcbin = zbtbin
zblbin = zbtbin
zblcbin = zblbin
zbrelbin = binD 0 20 0.05
zbrelcbin = zbrelbin


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


zbtc, zblc, zbrelc :: (HasSVConstits a, HasPVConstits a) => a -> PhysObj Double
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


zbrelc j = do
  svp3 <- toXYZ . fold <$> svChargedConstits j
  p3 <- toXYZ <$> chargedSum j
  let denom = modulus2 p3
      num = modulus $ svp3 `cross` p3
  return $ num / denom


zbt, zbl, zbrel :: (HasSVConstits a, HasPVConstits a) => a -> PhysObj Double
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


zbrel j = do
  svp3 <- toXYZ . fold <$> svConstits j
  p3 <- toXYZ <$> constitsSum j
  let denom = modulus2 p3
      num = modulus $ svp3 `cross` p3
  return $ num / denom


chargedPtH :: (HasSVConstits a, HasPVConstits a) => VarFill a
chargedPtH = h =$<< fmap (view lvPt) . chargedSum
  where
    h = hist1DDef (binD 0 25 250) "charged $p_{\\mathrm T}$ [GeV]" (dsigdXpbY pt gev)


pvPtH :: HasPVConstits a => VarFill a
pvPtH = h =$<< fmap (view lvPt . fold) . pvConstits
  where h = hist1DDef (binD 0 25 250) "PV $p_{\\mathrm T}$ [GeV]" (dsigdXpbY pt gev)


pvPtcH :: HasPVConstits a => VarFill a
pvPtcH = h =$<< fmap (view lvPt . fold) . pvChargedConstits
  where
    h =
      hist1DDef
        (binD 0 25 250)
        "PV charged $p_{\\mathrm T}$ [GeV]"
        (dsigdXpbY pt gev)


svPtH :: HasSVConstits a => VarFill a
svPtH = h =$<< fmap (view lvPt . fold) . svConstits
  where h = hist1DDef (binD 0 25 250) "SV $p_{\\mathrm T}$ [GeV]" (dsigdXpbY pt gev)


svPtcH :: HasSVConstits a => VarFill a
svPtcH = h =$<< fmap (view lvPt . fold) . svChargedConstits
  where
    h =
      hist1DDef
        (binD 0 25 250)
        "SV charged $p_{\\mathrm T}$"
        (dsigdXpbY pt gev)


svMH :: HasSVConstits a => VarFill a
svMH = h =$<< fmap (view lvM . fold) . svConstits
  where
    h = hist1DDef (binD 0 20 10) "SV mass [GeV]" (dsigdXpbY "m" gev)


svMcH :: HasSVConstits a => VarFill a
svMcH = h =$<< fmap (view lvM . fold) . svChargedConstits
  where
    h = hist1DDef (binD 0 20 10) "SV charged mass [GeV]" (dsigdXpbY "m" gev)


zbtH :: (HasSVConstits a, HasPVConstits a) => VarFill a
zbtH = h =$<< zbt
  where
    h = hist1DDef zbtbin zbtname (dsigdXpbY zbtname "1")


zbtcH :: (HasSVConstits a, HasPVConstits a) => VarFill a
zbtcH = h =$<< zbtc
  where
    h = hist1DDef zbtcbin zbtcname (dsigdXpbY zbtcname "1")


zblH :: (HasSVConstits a, HasPVConstits a) => VarFill a
zblH = h =$<< zbl
  where
    h = hist1DDef zblbin zblname (dsigdXpbY zblname "1")


zblcH :: (HasSVConstits a, HasPVConstits a) => VarFill a
zblcH = h =$<< zblc
  where
    h = hist1DDef zblcbin zblcname (dsigdXpbY zblcname "1")


zbrelH :: (HasSVConstits a, HasPVConstits a) => VarFill a
zbrelH = h =$<< zbrel
  where
    h = hist1DDef zbrelbin zbrelname (dsigdXpbY zbrelname "1")



zbrelcH :: (HasSVConstits a, HasPVConstits a) => VarFill a
zbrelcH = h =$<< zbrelc
  where
    h = hist1DDef zbrelcbin zbrelcname (dsigdXpbY zbrelcname "1")


nPVTracksH :: HasPVConstits a => VarFill a
nPVTracksH = h =$<< fmap (fromIntegral . length) . pvChargedConstits
  where
    h = hist1DDef npvtrkbin npvtrkname (dsigdXpbY npvtrkname "1")


nSVTracksH :: (HasLorentzVector a, HasSVConstits a) => VarFill a
nSVTracksH = h =$<< fmap (fromIntegral . length) . svChargedConstits
  where
    h = hist1DDef nsvtrkbin nsvtrkname (dsigdXpbY nsvtrkname "1")


zblcVszbtcH :: (HasPVConstits a, HasSVConstits a) => VarFill a
zblcVszbtcH = h =$<< (\j -> (,) <$> zblc j <*> zbtc j)
  where
    h = hist2DDef zblcbin zbtcbin zblcname zbtcname


zblcVszbrelcH :: (HasPVConstits a, HasSVConstits a) => VarFill a
zblcVszbrelcH = h =$<< (\j -> (,) <$> zblc j <*> zbrelc j)
  where
    h = hist2DDef zblcbin zbrelcbin zblcname zbrelcname


zbtcVszbrelcH :: (HasPVConstits a, HasSVConstits a) => VarFill a
zbtcVszbrelcH = h =$<< (\j -> (,) <$> zbtc j <*> zbrelc j)
  where
    h = hist2DDef zbtcbin zbrelcbin zbtcname zbrelcname


bfragHs :: (HasPVConstits a, HasSVConstits a, HasLorentzVector a) => VarFills a
bfragHs =
  mconcat
  [ singleton "/zbt" <$> zbtH
  , singleton "/zbtc" <$> zbtcH
  , singleton "/zbl" <$> zblH
  , singleton "/zblc" <$> zblcH
  , singleton "/zbrel" <$> zbrelH
  , singleton "/zbrelc" <$> zbrelcH
  , singleton "/chargedpt" <$> chargedPtH
  , singleton "/pvpt" <$> pvPtH
  , singleton "/pvptc" <$> pvPtcH
  , singleton "/svpt" <$> svPtH
  , singleton "/svmass" <$> svMH
  , singleton "/svmassc" <$> svMcH
  , singleton "/svptc" <$> svPtcH
  , singleton "/npvtrk" <$> nPVTracksH
  , singleton "/nsvtrk" <$> nSVTracksH
  , singleton "/zblcvszbtc" <$> zblcVszbtcH
  , singleton "/zblcvszbrelc" <$> zblcVszbrelcH
  , singleton "/zbtcvszbrelc" <$> zbtcVszbrelcH

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
    --
    -- hs' = channelWithLabel "/5psvtrk" (fmap ((>= 5) . length) . svChargedConstits) hs
    -- hs'' = channelWithLabel "/4psvtrk" (fmap ((>= 4) . length) . svChargedConstits) hs


zbtcMerges, zblcMerges, zbrelcMerges, zbtMerges, zblMerges, zbrelMerges
  :: [[Int]]
zbtcMerges =
  [ [00, 01, 02, 03, 04, 05, 06, 07, 08, 09]
  , [10, 11, 12]
  , [13, 14]
  , [15, 16]
  , [17, 18]
  , [19, 20]
  ]

zbtMerges = zbtcMerges

zblcMerges = zbtcMerges
zblMerges = zblcMerges

zbrelcMerges =
  [ [00, 01]
  , [02, 03]
  , [04, 05]
  , [06, 07]
  , [08, 09, 10]
  , [11, 12, 13, 14, 15, 16, 17, 18, 19]
  ]

zbrelMerges = zbrelcMerges

obsTrimmers
  :: (Eq a1, Fractional a, Ord a, Monoid b, IsString a1)
  => a1 -> Histogram Vector (ArbBin a) b -> Histogram Vector (ArbBin a) b
obsTrimmers s =
  case s of
    "zbtc"   -> trimH zbtcMerges
    "zblc"   -> trimH zblcMerges
    "zbrelc" -> trimH zbrelcMerges
    "zbt"    -> trimH zbtMerges
    "zbl"    -> trimH zblMerges
    "zbrel"  -> trimH zbrelMerges
    _        -> id


obsNames
  :: (IsString t, IsString a, Eq a)
  => a -> (t, t, t, t)
obsNames s =
  case s of
    "zbtc"    -> (zbtcrecohname, zbtctruehname, zbtcrecomatchhname, zbtcmatrixname)
    "zblc"    -> (zblcrecohname, zblctruehname, zblcrecomatchhname, zblcmatrixname)
    "zbrelc"  -> (zbrelcrecohname, zbrelctruehname, zbrelcrecomatchhname, zbrelcmatrixname)
    "zbt"     -> (zbtrecohname, zbttruehname, zbtrecomatchhname, zbtmatrixname)
    "zbl"     -> (zblrecohname, zbltruehname, zblrecomatchhname, zblmatrixname)
    "zbrel"   -> (zbrelrecohname, zbreltruehname, zbrelrecomatchhname, zbrelmatrixname)
    _         -> error "unrecognized observable"


trimV :: Monoid a => [[Int]] -> Vector a -> Vector a
trimV = mergeV mappend mempty


trimB :: [[Int]] -> ArbBin a -> ArbBin a
trimB bm bs =
  foldl (flip $ uncurry mergeBinRange) bs . reverse
  $ (head &&& last) <$> bm


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


zbtmatrixname, zbtrecohname, zbtrecomatchhname, zbttruehname :: IsString s => s
zbtmatrixname = "/elmujjmatched/zbtmig"
zbtrecohname = "/elmujj/probejets/zbt"
zbtrecomatchhname = "/elmujjmatched/probejets/zbt"
zbttruehname = "/elmujjtrue/truejets/zbt"


zbtcmatrixname, zbtcrecohname, zbtcrecomatchhname, zbtctruehname
  :: IsString s => s
zbtcmatrixname = "/elmujjmatched/zbtcmig"
zbtcrecohname = "/elmujj/probejets/zbtc"
zbtcrecomatchhname = "/elmujjmatched/probejets/zbtc"
zbtctruehname = "/elmujjtrue/truejets/zbtc"


zblmatrixname, zblrecohname, zblrecomatchhname, zbltruehname
  :: IsString s => s
zblmatrixname = "/elmujjmatched/zblmig"
zblrecohname = "/elmujj/probejets/zbl"
zblrecomatchhname = "/elmujjmatched/probejets/zbl"
zbltruehname = "/elmujjtrue/truejets/zbl"


zblcmatrixname, zblcrecohname, zblcrecomatchhname, zblctruehname
  :: IsString s => s
zblcmatrixname = "/elmujjmatched/zblcmig"
zblcrecohname = "/elmujj/probejets/zblc"
zblcrecomatchhname = "/elmujjmatched/probejets/zblc"
zblctruehname = "/elmujjtrue/truejets/zblc"


zbrelmatrixname, zbrelrecohname, zbrelrecomatchhname, zbreltruehname
  :: IsString s => s
zbrelmatrixname = "/elmujjmatched/zbrelmig"
zbrelrecohname = "/elmujj/probejets/zbrel"
zbrelrecomatchhname = "/elmujjmatched/probejets/zbrel"
zbreltruehname = "/elmujjtrue/truejets/zbrel"


zbrelcmatrixname, zbrelcrecohname, zbrelcrecomatchhname, zbrelctruehname
  :: IsString s => s
zbrelcmatrixname = "/elmujjmatched/zbrelcmig"
zbrelcrecohname = "/elmujj/probejets/zbrelc"
zbrelcrecomatchhname = "/elmujjmatched/probejets/zbrelc"
zbrelctruehname = "/elmujjtrue/truejets/zbrelc"

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
