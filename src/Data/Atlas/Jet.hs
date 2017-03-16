{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Data.Atlas.Jet where

import           Control.Applicative      (ZipList (..))
import           Control.Lens
import           Data.Atlas.BFrag
import           Data.Atlas.Corrected
import           Data.Atlas.Histogramming
import           Data.Atlas.PtEtaPhiE
import           Data.Atlas.TruthJet
import           Data.Monoid              hiding ((<>))
import           Data.Semigroup
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


matchJTJ :: Jet -> [TruthJet] -> Maybe (Jet, TruthJet)
matchJTJ j tjs = getOption $ do
  Min (Arg dr tj) <-
    foldMap (\tj' -> Option . Just . Min $ Arg (lvDREta j tj') tj') tjs
  if dr < 0.3
    then return (j, tj)
    else Option Nothing


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
  mv2c10sfs :: ZipList (Vars SF) <-
    if isData
      then return . pure . pure $ sf "data" 1
      else
        imap (\i -> pure . sf ("btagSFjet" <> T.pack (show i))) . fmap float2Double
          <$> readBranch "JetBtagSF"

  let tagged =
        curry withCorrection
        <$> fmap (> 0.8244273) mv2c10s <*> mv2c10sfs


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
    if isData
      then return $ ZipList (repeat Nothing)
      else fmap (Just . flavFromCInt) <$> readBranch "JetTruthLabel"

  return . getZipList
    $ Jet
      <$> tlvs
      <*> mv2c10s
      <*> tagged
      <*> jvts
      <*> fmap getZipList pvtrks
      <*> fmap getZipList svtrks
      <*> flvs


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

jetHs :: Fills Jet
jetHs =
  channelsWithLabels
    [ ("/allJetFlavs", pure . const True)
    , ("/bottom", pure . bLabeled)
    , ("/notbottom", pure . notBLabeled)
    ]
  $ channelsWithLabels
    [ ("/2psvtrks", pure . (>= 2) . length . svTracks)
    , ("/2svtrks", pure . (== 2) . length . svTracks)
    , ("/3svtrks", pure . (== 3) . length . svTracks)
    , ("/4psvtrks", pure . (>= 4) . length . svTracks)
    ]
  $ channelsWithLabels
    ( ("/inclusive", pure . const True)
    : ("/pt_gt200", pure . (> 200) . view lvPt)
    : bins' "/pt" (view lvPt) [20, 30, 50, 75, 100, 150, 200]
    ++ bins' "/eta" (view lvAbsEta) [0, 0.5, 1.0, 1.5, 2.0, 2.5]
    )
  $ mconcat
    [ lvHs
    , mv2c10H
    , bfragHs
    ]

  where
    bins'
      :: T.Text
      -> (Jet -> Double)
      -> [Double]
      -> [(T.Text, Jet -> PhysObj Bool)]
    bins' lab f (b0:b1:bs) =
      ( fixT $ lab <> "_" <> T.pack (show b0) <> "_" <> T.pack (show b1)
      , pure . (\j -> let x = f j in b0 < x && x < b1)
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
