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
import           Control.Applicative (ZipList (..))
import           Control.Lens
import           Data.Bifunctor
import           Data.TTree
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

-- TODO
-- add Track datatype for holding z0, d0, etc...

data Jet =
  Jet
  { _jPtEtaPhiE    :: PtEtaPhiE
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


readJets :: (MonadIO m, MonadThrow m) => DataMC' -> TreeRead m [Jet]
readJets dmc = do
  tlvs <- lvsFromTTreeF "jet_pt" "jet_eta" "jet_phi" "jet_e"
  tagged' <-
    fmap ((> 0) . (fromEnum :: CChar -> Int)) <$> readBranch "jet_isbtagged_70"
  mv2c10s <- fmap float2Double <$> readBranch "jet_mv2c10"
  mv2c10sfs :: ZipList SF <-
    case dmc of
      Data'       -> pure $ pure mempty
      MC' NoVars  -> pure $ pure mempty
      MC' AllVars -> pure $ pure mempty
        -- TODO
        -- TODO
        -- imap (\i -> pure . sf ("btagSFjet" <> T.pack (show i))) . fmap float2Double
        --   <$> readBranch "JetBtagSF"

  let withsf x xsf = dictate xsf >> return x
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
          <*> fmap getZipList pvtrks
          <*> fmap getZipList svtrks
          <*> flvs

  return $ filter ((> 30) . view lvPt) js

jetTracksTLV
  :: (MonadIO m, MonadThrow m)
  => String
  -> String
  -> String
  -> String
  -> TreeRead m (ZipList (ZipList PtEtaPhiE))
jetTracksTLV spt seta sphi se = do
    trkpts <- (fmap.fmap) ((/1e3) . float2Double) . fromVVector <$> readBranch spt
    trketas <- (fmap.fmap) float2Double . fromVVector <$> readBranch seta
    trkphis <- (fmap.fmap) float2Double . fromVVector <$> readBranch sphi
    trkes <- (fmap.fmap) ((/1e3) . float2Double) . fromVVector <$> readBranch se

    let trks = V.zipWith4
            ( \pts etas phis es ->
                V.toList $ V.zipWith4 PtEtaPhiE pts etas phis es
            ) trkpts trketas trkphis trkes

    return . ZipList . fmap ZipList $ V.toList trks


mv2c10H :: Fills Jet
mv2c10H =
  fmap (singleton "/mv2c10") . physObjH
  $ hist1DDef (binD (-1) 25 1) "MV2c10" (dndx "\\mathrm{MV2c10}" "1")
    <$= first (view mv2c10)


-- jetHs :: Fills m m (Jet, Maybe TrueJet)
-- jetHs =
--   channelsWithLabels
--     [ ("/2psvtrks", pure . (>= 2) . length . svTracks . fst)
--     , ("/2svtrks", pure . (== 2) . length . svTracks . fst)
--     , ("/3svtrks", pure . (== 3) . length . svTracks . fst)
--     , ("/4svtrks", pure . (== 4) . length . svTracks . fst)
--     , ("/5svtrks", pure . (== 5) . length . svTracks . fst)
--     , ("/4psvtrks", pure . (>= 4) . length . svTracks . fst)
--     , ("/6psvtrks", pure . (>= 6) . length . svTracks . fst)
--     ]
--   $ channelsWithLabels
--     ( ("/ptgt40", pure . (> 40) . view lvPt . fst)
--       : ("/ptgt50", pure . (> 50) . view lvPt . fst)
--       : ("/ptgt75", pure . (> 75) . view lvPt . fst)
--       : pure ("/ptgt30", pure . const True)
--     -- : bins' "/pt" (view lvPt . fst) [20, 30, 50, 75, 100, 150, 200]
--     -- ++ bins' "/eta" (view lvAbsEta . fst) [0, 0.5, 1.0, 1.5, 2.0, 2.5]
--     )
--   $ channelsWithLabels
--     [ ("/allJets", pure . const True)
--     , ("/unmatched", pure . isNothing . snd)
--     ] allHs
--     `mappend`
--       channelWithLabel "/matched" (pure . isJust . snd) matchedHs
--
--   where
--     allHs = mconcat [lvHs , {- mv2c10H , -} bfragHs 21] <$= fst
--     matchedHs =
--       mappend
--         allHs
--         $ F.premap sequenceA (F.handles _Just recoVsTrueHs) <$= sequenceA

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
