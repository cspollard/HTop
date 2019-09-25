{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeFamilies              #-}

module BFrag.Event
  ( Event(..)
  , module X
  , runNumber, eventNumber
  , readRunNumber, readEventNumber, readRunEventNumber
  , recoEvent, trueEvent
  , eventHs
  ) where


import           Atlas
import           BFrag.BFrag            as X
import           BFrag.Electron         as X
import           BFrag.Jet              as X
import           BFrag.Muon             as X
import           BFrag.PtEtaPhiE        as X
import           BFrag.RecoEvent        as X
import           BFrag.TrueEvent        as X
import qualified Control.Foldl          as F
import           Control.Lens
import           Control.Monad          (join)
import           Data.HEP.LorentzVector as X
import           Data.Semigroup         (Arg (..), Min (..), Option (..), (<>))
import           Data.TTree
import           Data.Tuple             (swap)
import           GHC.Generics           (Generic)


data Event =
  Event
    { _runNumber   :: CUInt
    , _eventNumber :: CULong
    , _trueEvent   :: PhysObj TrueEvent
    , _recoEvent   :: PhysObj RecoEvent
    } deriving (Generic, Show)


trueEvent :: Lens' Event (PhysObj TrueEvent)
trueEvent = lens _trueEvent $ \e x -> e { _trueEvent = x }

recoEvent :: Lens' Event (PhysObj RecoEvent)
recoEvent = lens _recoEvent $ \e x -> e { _recoEvent = x }

runNumber :: Lens' Event CUInt
runNumber = lens _runNumber $ \e x -> e { _runNumber = x }

eventNumber :: Lens' Event CULong
eventNumber = lens _eventNumber $ \e x -> e { _eventNumber = x }

readRunNumber :: (MonadIO m, MonadThrow m) => TreeRead m CUInt
readRunNumber = readBranch "runNumber"

readEventNumber :: (MonadIO m, MonadThrow m) => TreeRead m CULong
readEventNumber = readBranch "eventNumber"

readRunEventNumber :: (MonadIO m, MonadThrow m) => TreeRead m (CUInt, CULong)
readRunEventNumber = (,) <$> readRunNumber <*> readEventNumber


eventHs :: VarFills Event
eventHs =
  mconcat
  [ recoEventHs =$<< view recoEvent
  , trueEventHs =$<< view trueEvent
  , matchedEventHs =$<< go
  ]

  where
    go evt = do
      tevt <- view trueEvent evt
      revt <- view recoEvent evt
      return (tevt, revt)


matchedEventHs :: VarFills (TrueEvent, RecoEvent)
matchedEventHs =
  channelWithLabel "/elmujjmatched" filt
  $ F.handles folded matchedJetHs <$= fmap join . collapsePO . fmap go

  where
    filt :: (TrueEvent, RecoEvent) -> PhysObj Bool
    filt (tevt, revt) = do
      x <- elmujj revt
      let y = elmujjTrue tevt
      return $ x && y


    go :: (TrueEvent, RecoEvent) -> [PhysObj (TrueJet, Jet)]
    go (tevt, revt) =
      let rjs = probeJets revt
          tjs = toListOf (trueJets . traverse . filtered trueBJet) tevt
          matches = fmap (fmap swap . sequence . trueMatch tjs) <$> rjs
      in (>>= fromMaybe') <$> matches

    fromMaybe' (Just x) = return x
    fromMaybe' Nothing  = poFail


trueMatch :: [TrueJet] -> Jet -> (Jet, Maybe TrueJet)
trueMatch tjs j = (j,) . getOption $ do
  Min (Arg dr tj) <-
    foldMap (\tj' -> Option . Just . Min $ Arg (lvDREta j tj') tj') tjs
  if dr < 0.3
    then return tj
    else Option Nothing


matchedJetHs :: VarFills (TrueJet, Jet)
matchedJetHs =
  -- channelsWithLabels
    -- [ ("/2precosvtrks", recoSVTrkCut (>= 2))
    -- , ("/2recosvtrks", recoSVTrkCut (== 2))
    -- , ("/3recosvtrks", recoSVTrkCut (== 3))
    -- , ("/4recosvtrks", recoSVTrkCut (== 4))
    -- [ ("", return . const True)
    -- , ("/4precosvtrks", recoSVTrkCut (>= 4))
    -- , ("/5precosvtrks", recoSVTrkCut (>= 5))
    -- , ("/6precosvtrks", recoSVTrkCut (>= 6))
    -- ]
    -- . channelsWithLabels
    --   [ ("/recoptgt30", recoPtCut 30)
    --   , ("/recoptgt40", recoPtCut 40)
    --   , ("/recoptgt50", recoPtCut 50)
    --   , ("/recoptgt75", recoPtCut 75)
      -- ]
    -- . mconcat
  mconcat
  $ (prefixF "/probejets" <$> bfragHs <$= fmap snd)
    : (prefixF "/truejets" <$> bfragHs <$= fmap fst)
    : [ zbtMig, zbtcMig, zblMig, zblcMig, zbrelMig, zbrelcMig
      , nsvtrkMig, npvtrkMig
      , zbtDiff, zbtcDiff, zblDiff, zblcDiff, zbrelDiff, zbrelcDiff
      , nsvtrkDiff, npvtrkDiff, nsvtrkRelDiff, npvtrkRelDiff
      -- , svPtDiff
      -- , svPtcDiff
      -- , pvPtDiff
      -- , pvPtcDiff
      ]

  where
    recoSVTrkCut f = fmap (f . length) . svChargedConstits . snd
  --   recoPtCut ptMin = pure . (> ptMin) . view lvPt . snd


zbtMig :: VarFills (TrueJet, Jet)
zbtMig = singleton "/zbtmig" <$> h =$<< zbts
  where
    h = hist2DDef zbtbin zbtbin ("true " <> zbtname) ("reco " <> zbtname)


zbtcMig :: VarFills (TrueJet, Jet)
zbtcMig = singleton "/zbtcmig" <$> h =$<< zbtcs
  where
    h = hist2DDef zbtcbin zbtcbin ("true " <> zbtcname) ("reco " <> zbtcname)


zblMig :: VarFills (TrueJet, Jet)
zblMig = singleton "/zblmig" <$> h =$<< zbls
  where
    h = hist2DDef zblbin zblbin ("true " <> zblname) ("reco " <> zblname)


zblcMig :: VarFills (TrueJet, Jet)
zblcMig = singleton "/zblcmig" <$> h =$<< zblcs
  where
    h = hist2DDef zblcbin zblcbin ("true " <> zblcname) ("reco " <> zblcname)


zbrelMig :: VarFills (TrueJet, Jet)
zbrelMig = singleton "/zbrelmig" <$> h =$<< zbrels
  where
    h =
      hist2DDef
        zbrelbin
        zbrelbin
        ("true " <> zbrelname)
        ("reco " <> zbrelname)


zbrelcMig :: VarFills (TrueJet, Jet)
zbrelcMig = singleton "/zbrelcmig" <$> h =$<< zbrelcs
  where
    h =
      hist2DDef
        zbrelcbin
        zbrelcbin
        ("true " <> zbrelcname)
        ("reco " <> zbrelcname)


zbtDiff :: VarFills (TrueJet, Jet)
zbtDiff = singleton "/zbtdiff" <$> h =$<< f
  where
    f (tj, rj) = do
      tz <- zbt tj
      rz <- zbt rj
      return $ tz - rz

    h =
      hist1DDef
        (evenBins' (-1) 50 1)
        ("(true - reco) " <> zbtname)
        (dsigdXpbY zbtname "1")


zbtcDiff :: VarFills (TrueJet, Jet)
zbtcDiff = singleton "/zbtcdiff" <$> h =$<< fmap (uncurry (-)) . zbtcs
  where
    h =
      hist1DDef
        (evenBins' (-1) 50 1)
        ("(true - reco) " <> zbtcname)
        (dsigdXpbY zbtcname "1")


zblDiff :: VarFills (TrueJet, Jet)
zblDiff = singleton "/zbldiff" <$> h =$<< f
  where
    f (tj, rj) = do
      tz <- zbl tj
      rz <- zbl rj
      return $ tz - rz

    h =
      hist1DDef
        (evenBins' (-1) 50 1)
        ("(true - reco) " <> zblname)
        (dsigdXpbY zblname "1")


zblcDiff :: VarFills (TrueJet, Jet)
zblcDiff = singleton "/zblcdiff" <$> h =$<< fmap (uncurry (-)) . zblcs
  where
    h =
      hist1DDef
        (evenBins' (-1) 50 1)
        ("(true - reco) " <> zblcname)
        (dsigdXpbY zblcname "1")


zbrelDiff :: VarFills (TrueJet, Jet)
zbrelDiff = singleton "/zbreldiff" <$> h =$<< f
  where
    f (tj, rj) = do
      tz <- zbrel tj
      rz <- zbrel rj
      return $ tz - rz

    h =
      hist1DDef
        (evenBins' (-1) 50 1)
        ("(true - reco) " <> zbrelname)
        (dsigdXpbY zbrelname "1")


zbrelcDiff :: VarFills (TrueJet, Jet)
zbrelcDiff = singleton "/zbrelcdiff" <$> h =$<< fmap (uncurry (-)) . zbrelcs
  where
    h =
      hist1DDef
        (evenBins' (-1) 50 1)
        ("(true - reco) " <> zbrelcname)
        (dsigdXpbY zbrelcname "1")


nsvtrkDiff :: VarFills (TrueJet, Jet)
nsvtrkDiff = singleton "/nsvtrkdiff" <$> h =$<< fmap (uncurry (-)) . nsvtrks
  where
    h =
      hist1DDef
        (evenBins' (-10) 20 10)
        ("(true - reco) " <> nsvtrkname)
        (dsigdXpbY nsvtrkname "1")


npvtrkDiff :: VarFills (TrueJet, Jet)
npvtrkDiff = singleton "/npvtrkdiff" <$> h =$<< fmap (uncurry (-)) . npvtrks
  where
    h =
      hist1DDef
        (evenBins' (-10) 20 10)
        ("(true - reco) " <> npvtrkname)
        (dsigdXpbY npvtrkname "1")


nsvtrkRelDiff :: VarFills (TrueJet, Jet)
nsvtrkRelDiff = singleton "/nsvtrkreldiff" <$> (h =$<< f)
  where
    f :: (TrueJet, Jet) -> PhysObj Double
    f (tj, j) = do
      ntj <- fromIntegral . length <$> svChargedConstits tj
      nj <- fromIntegral . length <$> svChargedConstits j
      return $ (ntj - nj) / ntj

    h =
      hist1DDef
        (evenBins' (-1) 20 1)
        ("(true - reco) / true " <> nsvtrkname)
        (dsigdXpbY nsvtrkname "1")


npvtrkRelDiff :: VarFills (TrueJet, Jet)
npvtrkRelDiff = singleton "/npvtrkreldiff" <$> h =$<< f
  where
    f (tj, j) = do
      ntj <- fromIntegral . length <$> pvChargedConstits tj
      nj <- fromIntegral . length <$> pvChargedConstits j
      return $ (ntj - nj) / ntj

    h =
      hist1DDef
        (evenBins' (-1) 20 1)
        ("(true - reco) / true " <> npvtrkname)
        (dsigdXpbY npvtrkname "1")


zbs
  :: (forall a. (HasSVConstits a, HasPVConstits a) => a -> PhysObj Double)
  -> (TrueJet, Jet) -> PhysObj (Double, Double)
zbs f (tj, rj) = do
  tjz <- f tj
  rjz <- f rj
  return (tjz, rjz)


zbts, zbtcs, zbls, zblcs, zbrels, zbrelcs, nsvtrks, npvtrks
  :: (TrueJet, Jet) -> PhysObj (Double, Double)
zbts = zbs zbt
zbtcs = zbs zbtc
zbls = zbs zbl
zblcs = zbs zblc
zbrels = zbs zbrel
zbrelcs = zbs zbrelc
nsvtrks = zbs $ fmap (fromIntegral . length) . svChargedConstits
npvtrks = zbs $ fmap (fromIntegral . length) . pvChargedConstits


nsvtrkMig :: VarFills (TrueJet, Jet)
nsvtrkMig = singleton "/nsvtrkmig" <$> h =$<< nsvtrks
  where
    h = hist2DDef nsvtrkbin nsvtrkbin ("true " <> nsvtrkname) ("reco " <> nsvtrkname)


npvtrkMig :: VarFills (TrueJet, Jet)
npvtrkMig = singleton "/npvtrkmig" <$> h =$<< npvtrks
  where
    h = hist2DDef
        npvtrkbin
        npvtrkbin
        ("true " <> npvtrkname)
        ("reco " <> npvtrkname)
