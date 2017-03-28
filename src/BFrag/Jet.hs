{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}

module BFrag.Jet where

import           Atlas
import           BFrag.BFrag
import           BFrag.PtEtaPhiE
import           BFrag.Systematics
import           BFrag.TruthJet
import           Control.Applicative (ZipList (..))
import qualified Control.Foldl       as F
import           Control.Lens
import           Data.Maybe
import           Data.Monoid         hiding ((<>))
import           Data.Semigroup
import qualified Data.Text           as T
import           Data.TTree
import           Data.Tuple          (swap)
import qualified Data.Vector         as V
import           GHC.Float
import           GHC.Generics        (Generic)

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
    , _mv2c10      :: Double
    , _isBTagged   :: PhysObj Bool
    , _jvt         :: Double
    , _pvTrks      :: [PtEtaPhiE]
    , _svTrks      :: [PtEtaPhiE]
    , _truthFlavor :: Maybe JetFlavor
    } deriving (Generic, Show)

instance HasLorentzVector Jet where
    toPtEtaPhiE = lens _jPtEtaPhiE $ \j x -> j { _jPtEtaPhiE = x }

instance HasSVTracks Jet where
  svTracks = view svTrks

instance HasPVTracks Jet where
  pvTracks = view pvTrks


truthMatch :: [TruthJet] -> Jet -> (Jet, Maybe TruthJet)
truthMatch tjs j = (j,) . getOption $ do
  Min (Arg dr tj) <-
    foldMap (\tj' -> Option . Just . Min $ Arg (lvDREta j tj') tj') tjs
  if dr < 0.3
    then return tj
    else Option Nothing


jetTracksIsTight :: MonadIO m => TR m (ZipList (ZipList Bool))
jetTracksIsTight =
  ZipList
    . V.toList
    . fmap (ZipList . V.toList)
    . over (traverse.traverse) (/= (0 :: CInt))
    . fromVVector
    <$> readBranch "JetTracksisTight"


readJets :: MonadIO m => DataMC' -> TR m [Jet]
readJets dmc = do
  tlvs <- lvsFromTTreeF "JetPt" "JetEta" "JetPhi" "JetE"
  mv2c10s <- fmap float2Double <$> readBranch "JetMV2c10"
  mv2c10sfs :: ZipList (Vars SF) <-
    case dmc of
      Data' -> return . pure . pure $ sf "data" 1
      _ ->
        imap (\i -> pure . sf ("btagSFjet" <> T.pack (show i))) . fmap float2Double
          <$> readBranch "JetBtagSF"

  let withsf x xsf = onlySFVars xsf x
      tagged = withsf <$> fmap (> 0.8244273) mv2c10s <*> mv2c10sfs


  jvts <- fmap float2Double <$> readBranch "JetJVT"

  pvtrks' <-
    jetTracksTLV
      "JetTracksPt"
      "JetTracksEta"
      "JetTracksPhi"
      "JetTracksE"
  pvtrksTight <- jetTracksIsTight

  let f x y = ZipList . fmap snd . filter fst . getZipList $ (,) <$> x <*> y
      pvtrks = f <$> pvtrksTight <*> pvtrks'

  svtrks <-
    jetTracksTLV
      "JetSV1TracksPt"
      "JetSV1TracksEta"
      "JetSV1TracksPhi"
      "JetSV1TracksE"

  flvs <-
    case dmc of
      Data' -> return $ pure Nothing
      _     -> fmap (Just . flavFromCInt) <$> readBranch "JetTruthLabel"

  return . getZipList
    $ Jet
      <$> tlvs
      <*> mv2c10s
      <*> tagged
      <*> jvts
      <*> fmap getZipList pvtrks
      <*> fmap getZipList svtrks
      <*> flvs


-- TODO
-- this will need to be updated with AT ntuples!
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


mv2c10H :: Fills Jet
mv2c10H =
  singleton "/mv2c10"
  <$> hist1DDef (binD (-1) 25 1) "MV2c10" (dsigdXpbY "\\mathrm{MV2c10}" "1")
  <$= view mv2c10

zbtMigration :: Fills (Jet, TruthJet)
zbtMigration =
  singleton "/recozbtvstruezbt"
  <$> h
  <$= swap . bimap zBT zBT

  where
    h =
      hist2DDef
        (binD 0 21 1.05)
        (binD 0 21 1.05)
        "true $z_{p_{\\mathrm T}}$"
        "reco $z_{p_{\\mathrm T}}$"

nsvMigration :: Fills (Jet, TruthJet)
nsvMigration =
  singleton "/reconsvtrksvstruensvtrks"
  <$> h
  <$= swap . bimap (fromIntegral . nSVTracks) (fromIntegral . nSVTracks)

  where
    h =
      hist2DDef
        (binD 0 10 10)
        (binD 0 10 10)
        "true $n$ SV tracks"
        "reco $n$ SV tracks"

npvMigration :: Fills (Jet, TruthJet)
npvMigration =
  singleton "/reconpvtrksvstruenpvtrks"
  <$> h
  <$= swap . bimap (fromIntegral . nPVTracks) (fromIntegral . nPVTracks)

  where
    h =
      hist2DDef
        (binD 0 20 20)
        (binD 0 20 20)
        "true $n$ PV tracks"
        "reco $n$ PV tracks"

recoVsTruthHs :: Fills (Jet, TruthJet)
recoVsTruthHs = mconcat [zbtMigration, nsvMigration, npvMigration]


jetHs :: Fills (Jet, Maybe TruthJet)
jetHs =
  channelsWithLabels
    [ ("/2psvtrks", pure . (>= 2) . length . svTracks . fst)
    , ("/2svtrks", pure . (== 2) . length . svTracks . fst)
    , ("/3svtrks", pure . (== 3) . length . svTracks . fst)
    , ("/4svtrks", pure . (== 4) . length . svTracks . fst)
    , ("/5svtrks", pure . (== 5) . length . svTracks . fst)
    , ("/4psvtrks", pure . (>= 4) . length . svTracks . fst)
    , ("/6psvtrks", pure . (>= 6) . length . svTracks . fst)
    ]
  $ channelsWithLabels
    ( ("/ptgt40", pure . (> 40) . view lvPt . fst)
      : ("/ptgt50", pure . (> 50) . view lvPt . fst)
      : ("/ptgt75", pure . (> 75) . view lvPt . fst)
      : pure ("/ptgt30", pure . const True)
    -- : bins' "/pt" (view lvPt . fst) [20, 30, 50, 75, 100, 150, 200]
    -- ++ bins' "/eta" (view lvAbsEta . fst) [0, 0.5, 1.0, 1.5, 2.0, 2.5]
    )
  $ channelsWithLabels
    [ ("/allJets", pure . const True)
    , ("/unmatched", pure . isNothing . snd)
    ] allHs
    `mappend`
      channelWithLabel "/matched" (pure . isJust . snd) matchedHs

  where
    allHs = mconcat [lvHs , {- mv2c10H , -} bfragHs] <$= fst
    matchedHs =
      mappend
        allHs
        $ F.premap sequenceA (F.handles _Just recoVsTruthHs) <$= sequenceA

  -- where
    -- bins'
    --   :: T.Text
    --   -> ((Jet, a) -> Double)
    --   -> [Double]
    --   -> [(T.Text, (Jet, a) -> PhysObj Bool)]
    -- bins' lab f (b0:b1:bs) =
    --   ( fixT $ lab <> "_" <> T.pack (show b0) <> "_" <> T.pack (show b1)
    --   , pure . (\j -> let x = f j in b0 < x && x < b1)
    --   ) : bins' lab f (b1:bs)
    --
    -- bins' _ _ _ = []
    --
    -- fixT = T.replace "-" "m"

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

hasSV :: Jet -> Bool
hasSV = not . null . svTracks


-- TODO
-- macro here?
-- can't use template haskell
mv2c10 :: Lens' Jet Double
mv2c10 = lens _mv2c10 $ \j x -> j { _mv2c10 = x }

jvt :: Lens' Jet Double
jvt = lens _jvt $ \j x -> j { _jvt = x }


pvTrks, svTrks :: Lens' Jet [PtEtaPhiE]
pvTrks = lens _pvTrks $ \j x -> j { _pvTrks = x }
svTrks = lens _svTrks $ \j x -> j { _svTrks = x }

isBTagged :: Lens' Jet (PhysObj Bool)
isBTagged = lens _isBTagged $ \j x -> j { _isBTagged = x }

truthFlavor :: Lens' Jet (Maybe JetFlavor)
truthFlavor = lens _truthFlavor $ \j x -> j { _truthFlavor = x }
