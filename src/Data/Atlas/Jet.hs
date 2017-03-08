{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Data.Atlas.Jet where

import           Control.Applicative      (ZipList (..))
import           Control.Lens
import           Data.Atlas.Corrected
import           Data.Atlas.Histogramming
import           Data.Atlas.PtEtaPhiE
import           Data.Foldable            (fold)
import           Data.Monoid
import qualified Data.Text                as T
import           Data.TTree
import qualified Data.Vector              as V
import           GHC.Float
import           GHC.Generics             (Generic)

data JetFlavor = L | C | B | T
    deriving (Generic, Show, Eq, Ord)

flavFromCInt :: CInt -> JetFlavor
flavFromCInt x =
  case x of
    5  -> B
    4  -> C
    0  -> L
    15 -> T
    _  -> error $ "bad jet flavor label: " ++ show x



data Jet =
  Jet
    { _jPtEtaPhiE  :: PtEtaPhiE
    , _mv2c10      :: Corrected SF Double
    , _jvt         :: Double
    , _pvTracks    :: [PtEtaPhiE]
    , _pvTrkSum    :: PtEtaPhiE
    , _svTracks    :: [PtEtaPhiE]
    , _svTrkSum    :: PtEtaPhiE
    , _truthFlavor :: Maybe JetFlavor
    } deriving (Generic, Show)

instance HasLorentzVector Jet where
    toPtEtaPhiE = lens _jPtEtaPhiE $ \j x -> j { _jPtEtaPhiE = x }


jetTracksIsTight :: MonadIO m => TR m (ZipList (ZipList Bool))
jetTracksIsTight =
  ZipList
    . V.toList
    . fmap (ZipList . V.toList)
    . over (traverse.traverse) (/= (0 :: CInt))
    . fromVVector
    <$> readBranch "JetTracksisTight"


readJets :: MonadIO m => Bool -> TR m [Jet]
readJets isData = do
  tlvs <- lvsFromTTreeF "JetPt" "JetEta" "JetPhi" "JetE"
  mv2c10s <- fmap float2Double <$> readBranch "JetMV2c10"
  mv2c10sfs <- fmap (Product . float2Double) <$> readBranch "JetBtagSF"
  jvts <- fmap float2Double <$> readBranch "JetJVT"

  pvtrks' <- jetTracksTLV "JetTracksPt" "JetTracksEta" "JetTracksPhi" "JetTracksE"
  pvtrksTight <- jetTracksIsTight

  let f x y = ZipList . fmap snd . filter fst . getZipList $ (,) <$> x <*> y
      pvtrks = f <$> pvtrksTight <*> pvtrks'

  svtrks <- jetTracksTLV "JetSV1TracksPt" "JetSV1TracksEta" "JetSV1TracksPhi" "JetSV1TracksE"

  let pvtrksum = fold <$> pvtrks
      svtrksum = fold <$> svtrks
  flvs <-
    if isData
      then return $ ZipList (repeat Nothing)
      else fmap (Just . flavFromCInt) <$> readBranch "JetTruthLabel"

  return . getZipList
    $ Jet
      <$> tlvs
      <*> (curry withCorrection <$> mv2c10s <*> mv2c10sfs)
      <*> jvts
      <*> fmap getZipList pvtrks
      <*> pvtrksum
      <*> fmap getZipList svtrks
      <*> svtrksum
      <*> flvs


bFrag :: (Profunctor p, Contravariant f, Functor f) => Optic' p f Jet Double
-- protect against dividing by zero
bFrag = to $ \j ->
  let svtrksum = view (svTrkSum.lvPt) j
      trksum = svtrksum + view (pvTrkSum.lvPt) j
  in case trksum of
    0.0 -> 0.0
    x   -> svtrksum / x

trkSum :: Getter Jet PtEtaPhiE
trkSum = runGetter $ (<>) <$> Getter svTrkSum <*> Getter pvTrkSum


-- NB:
-- track info is already stored as doubles!
jetTracksTLV
  :: MonadIO m
  => String -> String -> String -> String -> TR m (ZipList (ZipList PtEtaPhiE))
jetTracksTLV spt seta sphi se = do
    trkpts <- fromVVector <$> readBranch spt
    trketas <- fromVVector <$> readBranch seta
    trkphis <- fromVVector <$> readBranch sphi
    trkes <- fromVVector <$> readBranch se

    let trks = V.zipWith4
            ( \pts etas phis es ->
                V.toList $ V.zipWith4 PtEtaPhiE pts etas phis es
            ) trkpts trketas trkphis trkes

    return . ZipList . fmap ZipList $ V.toList trks

-- histograms
-- TODO
-- how do we propogate the event weight down to the jet?
mv2c10H :: Fill Jet
mv2c10H =
  hist1DDef
    (binD (-1) 25 1)
    "MV2c10"
    (dsigdXpbY "\\mathrm{MV2c10}" "1")
    "/mv2c10"
    <$$= view mv2c10

trkSumPtH :: Fill Jet
trkSumPtH =
  hist1DDef
    (binD 0 25 500)
    "$p_{\\mathrm T} \\sum \\mathrm{trk}$"
    (dsigdXpbY pt gev)
    "/trksumpt"
    <$= view (trkSum.lvPt)

svTrkSumPtH :: Fill Jet
svTrkSumPtH =
  hist1DDef
    (binD 0 25 500)
    "$p_{\\mathrm T} \\sum \\mathrm{SV trk}$"
    (dsigdXpbY pt gev)
    "/svtrksumpt"
    <$= view (svTrkSum.lvPt)

bFragH :: Fill Jet
bFragH =
  hist1DDef
    (binD 0 22 1.1)
    "$z_{p_{\\mathrm T}}$"
    (dsigdXpbY "z_{p_{\\mathrm T}}" "1")
    "/bfrag"
    <$= view bFrag

tupGetter :: Getter s a -> Getter s b -> Getter s (a, b)
tupGetter f g = runGetter ((,) <$> Getter f <*> Getter g)

trkSumPtVsJetPtP :: Fill Jet
trkSumPtVsJetPtP =
  prof1DDef
    (binD 25 18 250)
    "$p_{\\mathrm T}$ [GeV]"
    "$<p_{\\mathrm T} \\sum \\mathrm{trk}>$"
    "/trksumptvsjetpt"
    <$= view (tupGetter lvPt (trkSum.lvPt))

svTrkSumPtVsJetPtP :: Fill Jet
svTrkSumPtVsJetPtP =
  prof1DDef
    (binD 25 18 250)
    "$p_{\\mathrm T}$ [GeV]"
    "$<p_{\\mathrm T} \\sum \\mathrm{SV trk}>$"
    "/svtrksumptvsjetpt"
    <$= view (tupGetter lvPt (svTrkSum.lvPt))

bFragVsJetPtP :: Fill Jet
bFragVsJetPtP =
  prof1DDef
    (binD 25 18 250)
    "$p_{\\mathrm T}$ [GeV]"
    "$<z_{p_{\\mathrm T}}>$"
    "/bfragvsjetpt"
    <$= view (tupGetter lvPt bFrag)

trkSumPtVsJetEtaP :: Fill Jet
trkSumPtVsJetEtaP =
  prof1DDef
    (binD 0 21 2.1)
    "$\\eta$"
    "$<p_{\\mathrm T} \\sum \\mathrm{trk}>$"
    "/trksumptvsjeteta"
    <$= view (tupGetter lvEta (trkSum.lvPt))

svTrkSumPtVsJetEtaP :: Fill Jet
svTrkSumPtVsJetEtaP =
  prof1DDef
    (binD 0 21 2.1)
    "$\\eta$"
    "$<p_{\\mathrm T} \\sum \\mathrm{SV trk}>$"
    "/svtrksumptvsjeteta"
    <$= view (tupGetter lvEta (svTrkSum.lvPt))

bFragVsJetEtaP :: Fill Jet
bFragVsJetEtaP =
  prof1DDef
    (binD 0 21 2.1)
    "$\\eta$"
    "$<z_{p_{\\mathrm T}}>$"
    "/bfragvsjeteta"
    <$= view (tupGetter lvEta bFrag)

svTrkSumPtVsTrkSumPtP :: Fill Jet
svTrkSumPtVsTrkSumPtP =
  prof1DDef
    (binD 0 10 100)
    "$p_{\\mathrm T} \\sum \\mathrm{trk}$"
    "$<p_{\\mathrm T} \\sum \\mathrm{SV trk}>$"
    "/svtrksumptvstrksumpt"
    <$= view (tupGetter (trkSum.lvPt) (svTrkSum.lvPt))

bFragVsTrkSumPtP :: Fill Jet
bFragVsTrkSumPtP =
  prof1DDef
    (binD 0 10 100)
    "$p_{\\mathrm T} \\sum \\mathrm{trk}$"
    "$<z_{p_{\\mathrm T}}>$"
    "/bfragvstrksumpt"
    <$= view (tupGetter (trkSum.lvPt) bFrag)

nPVTrksH :: Fill Jet
nPVTrksH =
  hist1DDef
    (binD 0 20 20)
    "$n$ PV tracks"
    (dsigdXpbY "n" "1")
    "/npvtrks"
    <$$= pure . fromIntegral . length . view pvTracks

nSVTrksH :: Fill Jet
nSVTrksH =
  hist1DDef
    (binD 0 10 10)
    "$n$ SV tracks"
    (dsigdXpbY "n" "1")
    "/nsvtrks"
    <$$= pure . fromIntegral . length . view svTracks

nPVTrksVsJetPtP :: Fill Jet
nPVTrksVsJetPtP =
  prof1DDef
    (binD 25 18 250)
    "$p_{\\mathrm T}$ [GeV]"
    "$<n$ PV tracks $>$"
    "/npvtrksvsjetpt"
    <$= view (tupGetter lvPt (pvTracks.to (fromIntegral.length)))

nSVTrksVsJetPtP :: Fill Jet
nSVTrksVsJetPtP =
  prof1DDef
    (binD 25 18 250)
    "$p_{\\mathrm T}$ [GeV]"
    "$<n$ SV tracks $>$"
    "/nsvtrksvsjetpt"
    <$= view (tupGetter lvPt (svTracks.to (fromIntegral.length)))

jetHs :: Fill Jet
jetHs =
  channels
    [ ("/allJetFlavs", const True)
    , ("/bottom", bLabeled)
    , ("/notbottom", notBLabeled)
    ]
  $ channels
    [ ("/2psvtrks", view (svTracks . lengthOfWith (>= 2)))
    , ("/2svtrks", view (svTracks . lengthOfWith (== 2)))
    , ("/3svtrks", view (svTracks . lengthOfWith (== 3)))
    , ("/4psvtrks", view (svTracks . lengthOfWith (>= 4)))
    ]
  $ channels
    ( ("/inclusive", const True)
    : ("/pt_gt200", (> 200) . view lvPt)
    : bins' "/pt" (view lvPt) [20, 30, 50, 75, 100, 150, 200]
    ++ bins' "/eta" (abs . view lvEta) [0, 0.5, 1.0, 1.5, 2.0, 2.5]
    )
  $ mconcat
    [ lvHs
    , mv2c10H
    , trkSumPtH
    , svTrkSumPtH
    , bFragH
    , trkSumPtVsJetPtP
    , svTrkSumPtVsJetPtP
    , bFragVsJetPtP
    , trkSumPtVsJetEtaP
    , svTrkSumPtVsJetEtaP
    , bFragVsJetEtaP
    , nPVTrksH
    , nSVTrksH
    , nPVTrksVsJetPtP
    , nSVTrksVsJetPtP
    ]

  where
    lengthOfWith f = to $ f . length

    bins' :: T.Text -> (Jet -> Double) -> [Double] -> [(T.Text, Jet -> Bool)]
    bins' lab f (b0:b1:bs) =
      ( fixT $ lab <> "_" <> T.pack (show b0) <> "_" <> T.pack (show b1)
      , \j -> let x = f j in b0 < x && x < b1
      ) : bins' lab f (b1:bs)

    bins' _ _ _ = []

    fixT = T.replace "-" "m"

bLabeled :: Jet -> Bool
bLabeled = views truthFlavor (== Just B)

cLabeled :: Jet -> Bool
cLabeled = views truthFlavor (== Just C)

lLabeled :: Jet -> Bool
lLabeled = views truthFlavor (== Just L)

tLabeled :: Jet -> Bool
tLabeled = views truthFlavor (== Just T)

notBLabeled :: Jet -> Bool
notBLabeled = getAny . views (truthFlavor._Just) (Any . (/= B))

bTagged :: Jet -> Corrected SF Bool
bTagged = views mv2c10 (fmap (> 0.8244273))

hasSV :: Jet -> Bool
hasSV = views svTracks (not . null)

-- TODO
-- macro here?
-- can't use template haskell
mv2c10 :: Lens' Jet (Corrected SF Double)
mv2c10 = lens _mv2c10 $ \j x -> j { _mv2c10 = x }

jvt :: Lens' Jet Double
jvt = lens _jvt $ \j x -> j { _jvt = x }


pvTracks, svTracks :: Lens' Jet [PtEtaPhiE]
pvTracks = lens _pvTracks $ \j x -> j { _pvTracks = x }
svTracks = lens _svTracks $ \j x -> j { _svTracks = x }

pvTrkSum, svTrkSum :: Lens' Jet PtEtaPhiE
pvTrkSum = lens _pvTrkSum $ \j x -> j { _pvTrkSum = x }
svTrkSum = lens _svTrkSum $ \j x -> j { _svTrkSum = x }

truthFlavor :: Lens' Jet (Maybe JetFlavor)
truthFlavor = lens _truthFlavor $ \j x -> j { _truthFlavor = x }
