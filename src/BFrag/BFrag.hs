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
import qualified Control.Foldl as F
import Control.Monad (join)


twine :: (c -> [PhysObj b]) -> VarFills b -> VarFills c
twine g h = F.handles folded h <$= fmap join . collapsePO . fmap g


zbtname, zblname, zbrelname :: Text
zbtname = "\\ensuremath{z_{\\mathrm{T,b}}}"
zblname = "\\ensuremath{z_{\\mathrm{L,b}}}"
zbrelname = "\\ensuremath{z_{\\mathrm{rel,b}}}"


zbtcname, zblcname, zbrelcname :: Text
zbtcname = "\\ensuremath{z_{\\mathrm{T,b}}^\\mathrm{ch}}"
zblcname = "\\ensuremath{z_{\\mathrm{L,b}}^\\mathrm{ch}}"
zbrelcname = "\\ensuremath{z_{\\mathrm{rel,b}}^\\mathrm{ch}}"

zbtcmatrixname, zbtcrecohname, zbtcrecomatchhname, zbtctruehname
  :: IsString s => s
zbtcmatrixname = "/elmujjmatched/zbtcmig"
zbtcrecohname = "/elmujj/probejets/zbtc"
zbtcrecomatchhname = "/elmujjmatched/probejets/zbtc"
zbtctruehname = "/elmujjtrue/truejets/zbtc"


zblcmatrixname, zblcrecohname, zblcrecomatchhname, zblctruehname
  :: IsString s => s
zblcmatrixname = "/elmujjmatched/zblcmig"
zblcrecohname = "/elmujj/probejets/zblc"
zblcrecomatchhname = "/elmujjmatched/probejets/zblc"
zblctruehname = "/elmujjtrue/truejets/zblc"


zbrelcmatrixname, zbrelcrecohname, zbrelcrecomatchhname, zbrelctruehname
  :: IsString s => s
zbrelcmatrixname = "/elmujjmatched/zbrelcmig"
zbrelcrecohname = "/elmujj/probejets/zbrelc"
zbrelcrecomatchhname = "/elmujjmatched/probejets/zbrelc"
zbrelctruehname = "/elmujjtrue/truejets/zbrelc"


nsvtrkmatrixname, nsvtrkrecohname, nsvtrkrecomatchhname, nsvtrktruehname
  :: IsString s => s
nsvtrkmatrixname = "/elmujjmatched/nsvtrkmig"
nsvtrkrecohname = "/elmujj/probejets/nsvtrk"
nsvtrkrecomatchhname = "/elmujjmatched/probejets/nsvtrk"
nsvtrktruehname = "/elmujjtrue/truejets/nsvtrk"


rhomatrixname, rhorecohname, rhorecomatchhname, rhotruehname
  :: IsString s => s
rhomatrixname = "/elmujjmatched/rhomig"
rhorecohname = "/elmujj/probejets/rho"
rhorecomatchhname = "/elmujjmatched/probejets/rho"
rhotruehname = "/elmujjtrue/truejets/rho"


zbtbin, zbtcbin, zblbin, zblcbin, zbrelbin, zbrelcbin :: BinD
zbtbin = binD 0 20 1
zbtcbin = zbtbin
zblbin = zbtbin
zblcbin = zblbin
zbrelbin = binD 0 20 0.02
zbrelcbin = zbrelbin


npvtrkname, nsvtrkname, msvname :: Text
npvtrkname = "\\ensuremath{n_{\\mathrm{PV}}^\\mathrm{ch}}"
nsvtrkname = "\\ensuremath{n_{\\mathrm{B}}^\\mathrm{ch}}"
msvname = "SV mass [GeV]"


npvtrkbin, nsvtrkbin, msvbin :: BinD
npvtrkbin = binD 0 20 20
nsvtrkbin = binD 3 8 11
msvbin = binD 0 12 6


rhoname :: Text
rhoname = "\\ensuremath{\\rho}"

rhobin :: BinD
rhobin = binD 0 20 2


class HasSVConstits a where
  svChargedTLV :: a -> PhysObj PtEtaPhiE
  svChargedTLV = fmap fold <$> svChargedConstits

  svChargedConstits :: a -> PhysObj [PtEtaPhiE]


class HasPVConstits a where
  pvChargedTLV :: a -> PhysObj PtEtaPhiE
  pvChargedTLV = fmap fold <$> pvChargedConstits

  pvChargedConstits :: a -> PhysObj [PtEtaPhiE]


chargedSum :: (HasSVConstits a, HasPVConstits a) => a -> PhysObj PtEtaPhiE
chargedSum j = do
  svt <- svChargedConstits j
  pvt <- pvChargedConstits j
  return . fold $ svt ++ pvt


fixOne :: (Ord a, Fractional a) => a -> a
fixOne x
  | x < 1 = x
  | x == 1 = 1 - 1e-9
  | otherwise = error "observable above unity!"


zbtc, zblc, zbrelc :: (HasSVConstits a, HasPVConstits a) => a -> PhysObj Double
zbtc j = do
  svp4 <- fold <$> svChargedConstits j
  p4 <- chargedSum j
  let denom = view lvPt p4
  case denom of
    0.0 -> empty
    x   -> pure . fixOne $ view lvPt svp4 / x


zblc j = do
  svp3 <- toXYZ . fold <$> svChargedConstits j
  p3 <- toXYZ <$> chargedSum j
  let denom = modulus2 p3
      num = svp3 `inner` p3
  case denom of
    0.0 -> empty
    x   -> pure . fixOne $ num / x


zbrelc j = do
  svp3 <- toXYZ . fold <$> svChargedConstits j
  p3 <- toXYZ <$> chargedSum j
  let denom = modulus2 p3
      num = modulus $ svp3 `cross` p3
  case denom of
    0.0 -> empty
    x   -> pure . fixOne $ num / x


localBins = binD 0 20 120

ptcH :: (HasSVConstits a, HasPVConstits a) => VarFill a
ptcH = h =$<< fmap (view lvPt) . chargedSum
  where
    h = hist1DDef localBins "charged $p_{\\mathrm T}$ [GeV]" (dsigdXpbY pt gev)


pvPtcH :: HasPVConstits a => VarFill a
pvPtcH = h =$<< fmap (view lvPt . fold) . pvChargedConstits
  where
    h =
      hist1DDef
        localBins
        "PV charged $p_{\\mathrm T}$ [GeV]"
        (dsigdXpbY pt gev)


svPtcH :: HasSVConstits a => VarFill a
svPtcH = h =$<< fmap (view lvPt . fold) . svChargedConstits
  where
    h =
      hist1DDef
        localBins
        "SV charged $p_{\\mathrm T}$"
        (dsigdXpbY pt gev)


svMcH :: HasSVConstits a => VarFill a
svMcH = h =$<< fmap (view lvM . fold) . svChargedConstits
  where
    h = hist1DDef msvbin "SV charged mass [GeV]" (dsigdXpbY "m" gev)


zbtcH :: (HasSVConstits a, HasPVConstits a) => VarFill a
zbtcH = h =$<< zbtc
  where
    h = hist1DDef zbtcbin zbtcname (dsigdXpbY zbtcname "1")


zblcH :: (HasSVConstits a, HasPVConstits a) => VarFill a
zblcH = h =$<< zblc
  where
    h = hist1DDef zblcbin zblcname (dsigdXpbY zblcname "1")


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


rhoH :: VarFill Double
rhoH = hist1DDef rhobin rhoname (dsigdXpbY rhoname "1")


bfragHs :: (HasPVConstits a, HasSVConstits a, HasLorentzVector a) => VarFills a
bfragHs =
  mconcat
  [ singleton "/zbtc" <$> zbtcH
  , singleton "/zblc" <$> zblcH
  , singleton "/zbrelc" <$> zbrelcH
  , singleton "/ptc" <$> ptcH
  , singleton "/pvptc" <$> pvPtcH
  , singleton "/svptc" <$> svPtcH
  , singleton "/svmc" <$> svMcH
  , singleton "/npvtrk" <$> nPVTracksH
  , singleton "/nsvtrk" <$> nSVTracksH
  ]


zbtcMerges, zblcMerges, zbrelcMerges, nsvtrkMerges, rhoMerges, rhoRecoMerges :: [[Int]]
zbtcMerges =
  [ [00, 01, 02, 03, 04, 05, 06, 07] ]
  ++ fmap andOne [08, 10, 12, 14, 16, 18]
  where
    andOne x = [x, x+1]

zblcMerges = zbtcMerges

zbrelcMerges =
  [ [00, 01]
  , [02, 03]
  , [04, 05, 06]
  , [07, 08, 09]
  , [10, 11, 12, 13, 14, 15, 16, 17, 18, 19]
  ]

nsvtrkMerges =
  [ [00]
  , [01]
  , [02]
  , [03]
  , [04, 05]
  , [06, 07]
  ]

rhoMerges =
  [ [00 .. 03]
  , [04, 05, 06]
  , [07, 08, 09]
  , [10, 11, 12]
  , [13, 14, 15]
  , [16, 17, 18, 19]
  ]

rhoRecoMerges =
  [00, 01]
  : (pure <$> [02 .. 09])
  ++ fmap andOne [10, 12, 14, 16, 18]
  where
    andOne x = [x, x+1]


-- combine n bins at a time up to a maximum of m
rebinBy :: Int -> Int -> [[Int]]
rebinBy n m = go <$> [0 .. m `div` n - 1]
  where
    go i = [i*n..(i+1)*n-1]



obsTruthTrimmers
  :: (Eq a1, Fractional a, Ord a, Monoid b, IsString a1)
  => a1 -> Histogram Vector (ArbBin a) b -> Histogram Vector (ArbBin a) b
obsTruthTrimmers s =
  case s of
    "zbtc"   -> trimH zbtcMerges
    "zblc"   -> trimH zblcMerges
    "zbrelc" -> trimH zbrelcMerges
    "zbtcnorm"   -> trimH zbtcMerges
    "zblcnorm"   -> trimH zblcMerges
    "zbrelcnorm" -> trimH zbrelcMerges
    "nsvtrk" -> trimH nsvtrkMerges
    "nsvtrknorm" -> trimH nsvtrkMerges
    "rho" -> trimH rhoMerges
    "rhonorm" -> trimH rhoMerges
    _        -> id


obsRecoTrimmers
  :: (Eq a1, Fractional a, Ord a, Monoid b, IsString a1, Show a1)
  => a1 -> Histogram Vector (ArbBin a) b -> Histogram Vector (ArbBin a) b
obsRecoTrimmers s =
  case s of
    "zbtc"   -> trimH $ [ [00, 01] ] ++ (pure <$> [02..19])
    "zblc"   -> trimH $ [ [00, 01] ] ++ (pure <$> [02..19])
    "zbrelc" -> trimH $ pure <$> [00..19]
    "zbtcnorm"   -> trimH $ [ [00, 01] ] ++ (pure <$> [02..19])
    "zblcnorm"   -> trimH $ [ [00, 01] ] ++ (pure <$> [02..19])
    "zbrelcnorm" -> trimH $ pure <$> [00..19]
    "rho" -> trimH rhoRecoMerges
    "rhonorm" -> trimH rhoRecoMerges
    "eta" -> trimH $ rebinBy 3 39
    "etanorm" -> trimH $ rebinBy 3 39
    "pt" -> trimH $ rebinBy 2 20
    "ptnorm" -> trimH $ rebinBy 2 20
    "ptc" -> trimH $ rebinBy 2 20
    "ptcnorm" -> trimH $ rebinBy 2 20
    "pvptc" -> trimH $ rebinBy 2 20
    "svptcnorm" -> trimH $ rebinBy 2 20
    "svptc" -> trimH $ rebinBy 2 20
    "pvptcnorm" -> trimH $ rebinBy 2 20
    _         -> id




obsNames
  :: (IsString t, IsString a, Eq a, Show a)
  => a -> (t, t, t)
obsNames s =
  case s of
    "zbtc"    -> (zbtcrecohname, zbtctruehname, zbtcmatrixname)
    "zblc"    -> (zblcrecohname, zblctruehname, zblcmatrixname)
    "zbrelc"  -> (zbrelcrecohname, zbrelctruehname, zbrelcmatrixname)
    "nsvtrk"  -> (nsvtrkrecohname, nsvtrktruehname, nsvtrkmatrixname)
    "rho"     -> (rhorecohname, rhotruehname, rhomatrixname)
    x         -> error $ "unrecognized observable" ++ show x


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


