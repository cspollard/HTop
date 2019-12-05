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
  , singleton "/zblcvszbtc" <$> zblcVszbtcH
  , singleton "/zblcvszbrelc" <$> zblcVszbrelcH
  , singleton "/zbtcvszbrelc" <$> zbtcVszbrelcH
  ]


zbtcMerges, zblcMerges, zbrelcMerges, zbtMerges, zblMerges, zbrelMerges
  :: [[Int]]
zbtcMerges =
  [ [00, 01, 02, 03, 04, 05, 06, 07, 08]
  , [09, 10, 11]
  , [12, 13]
  , [14, 15]
  , [16, 17]
  , [18, 19]
  ]

zbtMerges = zbtcMerges

zblcMerges = zbtcMerges
zblMerges = zblcMerges

zbrelcMerges =
  [ [00, 01]
  , [02, 03]
  , [04, 05, 06, 07, 08, 09]
  , [10, 11, 12, 13, 14, 15, 16, 17, 18, 19]
  ]

zbrelMerges = zbrelcMerges


obsTruthTrimmers
  :: (Eq a1, Fractional a, Ord a, Monoid b, IsString a1)
  => a1 -> Histogram Vector (ArbBin a) b -> Histogram Vector (ArbBin a) b
obsTruthTrimmers s =
  case s of
    "zbtc"   -> trimH zbtcMerges
    "zblc"   -> trimH zblcMerges
    "zbrelc" -> trimH zbrelcMerges
    "zbt"    -> trimH zbtMerges
    "zbl"    -> trimH zblMerges
    "zbrel"  -> trimH zbrelMerges
    _        -> id


obsRecoTrimmers
  :: (Eq a1, Fractional a, Ord a, Monoid b, IsString a1, Show a1)
  => a1 -> Histogram Vector (ArbBin a) b -> Histogram Vector (ArbBin a) b
obsRecoTrimmers s =
  case s of
    "zbtc"   -> trimH $ [ [00, 01] ] ++ (pure <$> [02..19])
    "zblc"   -> trimH $ [ [00, 01] ] ++ (pure <$> [02..19])
    "zbt"    -> trimH $ [ [00, 01] ] ++ (pure <$> [02..19])
    "zbl"    -> trimH $ [ [00, 01] ] ++ (pure <$> [02..19])
    "zbrelc" -> trimH $ pure <$> [00..19]
    "zbrel"  -> trimH $ pure <$> [00..19]
    _         -> id


obsNames
  :: (IsString t, IsString a, Eq a, Show a)
  => a -> (t, t, t, t)
obsNames s =
  case s of
    "zbtc"    -> (zbtcrecohname, zbtctruehname, zbtcrecomatchhname, zbtcmatrixname)
    "zblc"    -> (zblcrecohname, zblctruehname, zblcrecomatchhname, zblcmatrixname)
    "zbrelc"  -> (zbrelcrecohname, zbrelctruehname, zbrelcrecomatchhname, zbrelcmatrixname)
    "zbt"     -> (zbtrecohname, zbttruehname, zbtrecomatchhname, zbtmatrixname)
    "zbl"     -> (zblrecohname, zbltruehname, zblrecomatchhname, zblmatrixname)
    "zbrel"   -> (zbrelrecohname, zbreltruehname, zbrelrecomatchhname, zbrelmatrixname)
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
