{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module BFrag.Jet where

import           Atlas
import           BFrag.BFrag
import Data.Maybe (fromMaybe)
import           BFrag.PtEtaPhiE
import           BFrag.Systematics
import           BFrag.TrueJet
import           Control.Applicative    (ZipList (..))
import           Control.Lens
import           Control.Monad.Writer
import           Data.Foldable          (fold)
import           Data.Semigroup         (Any (..), Arg (..), Min (..),
                                         Option (..))
import           Data.TTree
import qualified Data.Vector            as V
import           GHC.Float
import           GHC.Generics           (Generic)


data JetFlavor = L | C | B | T
    deriving (Generic, Show, Eq, Ord)


-- TODO
-- partial
flavFromCInt :: CInt -> JetFlavor
flavFromCInt x =
  case x of
    5  -> B
    4  -> C
    0  -> L
    15 -> T
    _  -> error $ "bad jet flavor label: " ++ show x

flavToNum :: Num a => JetFlavor -> a
flavToNum x =
  case x of
    B -> 5
    C -> 4
    L -> 0
    T -> 15


data Jet =
  Jet
  { _jPtEtaPhiE  :: PtEtaPhiE
  , _mv2c10      :: Double
  , _isBTagged   :: PhysObj Bool
  , _jvt         :: Double
  , _pvTrks      :: PhysObj [PtEtaPhiE]
  , _svTrks      :: PhysObj [PtEtaPhiE]
  , _truthFlavor :: Maybe JetFlavor
  } deriving (Generic, Show)


instance HasLorentzVector Jet where
    toPtEtaPhiE = lens _jPtEtaPhiE $ \j x -> j { _jPtEtaPhiE = x }


instance HasSVConstits Jet where
  svConstits = view svTrks
  svChargedConstits = svConstits


instance HasPVConstits Jet where
  pvConstits = view pvTrks
  pvChargedConstits = pvConstits



appSVRW :: DataMC' -> Maybe BHadron -> [PtEtaPhiE] -> PhysObj ()
appSVRW Data' _ _ = return ()
appSVRW (MC' NoVars) _ _ = return ()
appSVRW _ Nothing _ = return ()
appSVRW _ (Just bh) svtrks = do
  -- note that the binning used in the reweighting histograms is in MeV!!!
  let svp4 = fold svtrks
      bhpt = view lvPt bh * 1e3
      dphi = lvDPhi bh svp4
      deta = lvDEta bh svp4
      dpt = bhpt - (view lvPt svp4 * 1e3)
      nsvtrks = fromIntegral $ length svtrks

      -- these are variations around zero.
      svEffVars = (+1) . flip safeAt bhpt <$> svEffSysts
      phiResVars = (+1) . flip safeAt dphi <$> phiResSysts
      etaResVars = (+1) . flip safeAt deta <$> etaResSysts
      ptResVars = (+1) . flip safeAt2 (bhpt, dpt) <$> ptResSysts

      -- this one is already the ratio data/simulation
      nsvtrkRW = safeAt nsvtrkSyst nsvtrks


  varSF $ sf "sveffsf" <$> Variation 1.0 svEffVars
  varSF $ sf "svphiressf" <$> Variation 1.0 phiResVars
  varSF $ sf "svetaressf" <$> Variation 1.0 etaResVars
  varSF $ sf "svptressf" <$> Variation 1.0 ptResVars
  varSF $ sf "nsvtrksf" <$> Variation 1.0 [("nsvtrksf", nsvtrkRW)]


appPVRW :: DataMC' -> [PtEtaPhiE] -> PhysObj ()
appPVRW Data' _ = return ()
appPVRW (MC' NoVars) _ = return ()
appPVRW _ pvtrks = do
  let npvtrks = fromIntegral $ length pvtrks
      -- this one is already the ratio data/simulation
      npvtrkRW = safeAt npvtrkSyst npvtrks

  varSF $ sf "npvtrksf" <$> Variation 1.0 [("npvtrksf", npvtrkRW)]


appPVSVRW :: DataMC' -> Double -> Double -> PhysObj ()
appPVSVRW Data' _ _ = return ()
appPVSVRW (MC' NoVars) _ _ = return ()
appPVSVRW _ jpt trkpt = do
  let -- note that the binning used in the reweighting histograms is in MeV!!!
      trkVars = (+1) . flip safeAt2 (jpt*1e3, trkpt*1e3) <$> sumPtTrkSysts

  varSF $ sf "trkptsf" <$> Variation 1.0 trkVars


safeAt :: (Num a, Ord x) => Binned x a -> x -> a
safeAt = atDefault 0


safeAt2 :: (Num a, Ord x, Ord y) => Binned y (Binned x a) -> (x, y) -> a
safeAt2 b (x, y) = flip safeAt x $ atDefault (Binned [] mempty) b y


atDefault :: Ord x => a -> Binned x a -> x -> a
atDefault a b = fromMaybe a . atBin b


matchBH :: [BHadron] -> PtEtaPhiE -> Maybe BHadron
matchBH bhs j = getOption $ do
  Min (Arg dr bh) <-
    foldMap (\bh' -> Option . Just . Min $ Arg (lvDREta j bh') bh') bhs
  if dr < 0.3
    then return bh
    else Option Nothing


readJets :: (MonadIO m, MonadThrow m) => DataMC' -> [BHadron] -> TreeRead m [Jet]
readJets dmc bhs = do
  tlvs <- lvsFromTTreeF "jet_pt" "jet_eta" "jet_phi" "jet_e"
  tagged' <-
    fmap ((> 0) . (fromEnum :: CChar -> Int)) <$> readBranch "jet_isbtagged_70"
  mv2c10s <- fmap float2Double <$> readBranch "jet_mv2c10"
  mv2c10sfs :: ZipList SF <-
    case dmc of
      Data'       -> pure $ pure mempty
      MC' NoVars  -> pure $ pure mempty
      MC' AllVars -> pure $ pure mempty
        -- imap (\i -> sf ("btagSFjet" <> T.pack (show i))) . fmap float2Double
        -- <$> readBranch "JetBtagSF"

  let withsf x xsf = writer (x, xsf)
      tagged = withsf <$> tagged' <*> mv2c10sfs

  jvts <- fmap float2Double <$> readBranch "jet_jvt"

  pvtrks <-
    jetTracksTLV
      "jet_pv_track_pt"
      "jet_pv_track_eta"
      "jet_pv_track_phi"
      "jet_pv_track_e"

  svtrks <-
    jetTracksTLV
      "jet_sv1_track_pt"
      "jet_sv1_track_eta"
      "jet_sv1_track_phi"
      "jet_sv1_track_e"

  -- apply the various systematic reweightings...
  let mbhs = matchBH bhs <$> tlvs
      jpts = view lvPt <$> tlvs
      pvsvpts = (\pv sv -> view lvPt (fold pv <> fold sv)) <$> pvtrks <*> svtrks
      pvsvrws :: ZipList (PhysObj ())
      pvsvrws = appPVSVRW dmc <$> jpts <*> pvsvpts
      pvtrks' :: ZipList (PhysObj [PtEtaPhiE])
      pvtrks' =
        (\pvsvrw pvt -> appPVRW dmc pvt >> pvsvrw >> return pvt)
        <$> pvsvrws
        <*> pvtrks
      svtrks' :: ZipList (PhysObj [PtEtaPhiE])
      svtrks' =
        (\pvsvrw mbh svt -> appSVRW dmc mbh svt >> pvsvrw >> return svt)
        <$> pvsvrws
        <*> mbhs
        <*> svtrks


  flvs <-
    case dmc of
      Data' -> return $ pure Nothing
      _     -> fmap (Just . flavFromCInt) <$> readBranch "jet_truthflav"

  let js = getZipList
        $ Jet
          <$> tlvs
          <*> mv2c10s
          <*> tagged
          <*> jvts
          <*> pvtrks'
          <*> svtrks'
          <*> flvs

  return js


jetTracksTLV
  :: (MonadIO m, MonadThrow m)
  => String
  -> String
  -> String
  -> String
  -> TreeRead m (ZipList [PtEtaPhiE])
jetTracksTLV spt seta sphi se = do
    trkpts <- (fmap.fmap) ((/1e3) . float2Double) . fromVVector <$> readBranch spt
    trketas <- (fmap.fmap) float2Double . fromVVector <$> readBranch seta
    trkphis <- (fmap.fmap) float2Double . fromVVector <$> readBranch sphi
    trkes <- (fmap.fmap) ((/1e3) . float2Double) . fromVVector <$> readBranch se

    let trks =
          V.zipWith4
            ( \pts etas phis es ->
                V.toList $ V.zipWith4 PtEtaPhiE pts etas phis es
            ) trkpts trketas trkphis trkes

    return . ZipList $ V.toList trks


mv2c10H :: Fills Jet
mv2c10H = h <$= fmap (view mv2c10)
  where
    h = histo1DDef (evenBins' (-1) 25 1) "MV2c10" (dsigdXpbY "\\mathrm{MV2c10}" "1") "/mv2c10"


hadronLabelH :: Fills Jet
hadronLabelH = h =$<< go
  where
    h = histo1DDef (evenBins' 0 16 16) "hadron label" (dsigdXpbY "\\mathrm{label}" "1") "/hadronlabel"
    go j =
      case view truthFlavor j of
        Nothing -> poFail
        Just f  -> return $ flavToNum f


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

hasSV :: Jet -> PhysObj Bool
hasSV j = do
  svtrks <- view svTrks j
  return $
    case svtrks of
      (_:_:_) -> True
      _       -> False

mv2c10 :: Lens' Jet Double
mv2c10 = lens _mv2c10 $ \j x -> j { _mv2c10 = x }

jvt :: Lens' Jet Double
jvt = lens _jvt $ \j x -> j { _jvt = x }


pvTrks, svTrks :: Lens' Jet (PhysObj [PtEtaPhiE])
pvTrks = lens _pvTrks $ \j x -> j { _pvTrks = x }
svTrks = lens _svTrks $ \j x -> j { _svTrks = x }

isBTagged :: Lens' Jet (PhysObj Bool)
isBTagged = lens _isBTagged $ \j x -> j { _isBTagged = x }

truthFlavor :: Lens' Jet (Maybe JetFlavor)
truthFlavor = lens _truthFlavor $ \j x -> j { _truthFlavor = x }
