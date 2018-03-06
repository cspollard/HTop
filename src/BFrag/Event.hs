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


eventHs :: Fills Event
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


matchedEventHs :: Fills (TrueEvent, RecoEvent)
matchedEventHs =
  channelWithLabel "/elmujjmatched" filt
  $ F.handles folded matchedJetHs <$= fmap join . sequenceL . fmap go

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
      in (fromMaybe' =<<) . fmap (fmap swap . sequence . trueMatch tjs) <$> rjs

    fromMaybe' (Just x) = return x
    fromMaybe' Nothing  = confess mempty


trueMatch :: [TrueJet] -> Jet -> (Jet, Maybe TrueJet)
trueMatch tjs j = (j,) . getOption $ do
  Min (Arg dr tj) <-
    foldMap (\tj' -> Option . Just . Min $ Arg (lvDREta j tj') tj') tjs
  if dr < 0.3
    then return tj
    else Option Nothing


matchedJetHs :: Fills (TrueJet, Jet)
matchedJetHs =
  -- channelsWithLabels
    -- [ ("/2precosvtrks", recoSVTrkCut (>= 2))
    -- , ("/2recosvtrks", recoSVTrkCut (== 2))
    -- , ("/3recosvtrks", recoSVTrkCut (== 3))
    -- , ("/4recosvtrks", recoSVTrkCut (== 4))
    -- , ("/4precosvtrks", recoSVTrkCut (>= 4))
    -- , ("/5recosvtrks", recoSVTrkCut (== 5))
    -- , ("/6precosvtrks", recoSVTrkCut (>= 6))
    -- ]
    -- . channelsWithLabels
    --   [ ("/recoptgt30", recoPtCut 30)
    --   , ("/recoptgt40", recoPtCut 40)
    --   , ("/recoptgt50", recoPtCut 50)
    --   , ("/recoptgt75", recoPtCut 75)
    --   ]
    -- . mconcat
    mconcat
    $ (prefixF "/probejets" <$> bfragHs <$= fmap snd)
      : (prefixF "/truejets" <$> bfragHs <$= fmap fst)
      : [ zbtMig, zbtcMig, zblMig, zblcMig, zbrelMig, zbrelcMig
        , nsvtrkMig, npvtrkMig
        , zbtDiff, zbtcDiff, zblDiff, zblcDiff, zbrelDiff, zbrelcDiff
        , nsvtrkDiff, npvtrkDiff
        -- , svPtDiff
        -- , svPtcDiff
        -- , pvPtDiff
        -- , pvPtcDiff
        ]

  -- where
  --   recoSVTrkCut f = fmap (f . length) . svChargedConstits . snd
  --   recoPtCut ptMin = pure . (> ptMin) . view lvPt . snd


zbtMig :: Fills (TrueJet, Jet)
zbtMig = singleton "/zbtmig" <$> physObjH h =$<< zbts
  where
    h = hist2DDef zbtbin zbtbin ("true " <> zbtname) ("reco " <> zbtname)

zbtcMig :: Fills (TrueJet, Jet)
zbtcMig = singleton "/zbtcmig" <$> physObjH h =$<< zbtcs
  where
    h = hist2DDef zbtcbin zbtcbin ("true " <> zbtcname) ("reco " <> zbtcname)

zblMig :: Fills (TrueJet, Jet)
zblMig = singleton "/zbtmig" <$> physObjH h =$<< zbls
  where
    h = hist2DDef zblbin zblbin ("true " <> zblname) ("reco " <> zblname)

zblcMig :: Fills (TrueJet, Jet)
zblcMig = singleton "/zblcmig" <$> physObjH h =$<< zblcs
  where
    h = hist2DDef zblcbin zblcbin ("true " <> zblcname) ("reco " <> zblcname)

zbrelMig :: Fills (TrueJet, Jet)
zbrelMig = singleton "/zbtmig" <$> physObjH h =$<< zbrels
  where
    h =
      hist2DDef
        zbrelbin
        zbrelbin
        ("true " <> zbrelname)
        ("reco " <> zbrelname)

zbrelcMig :: Fills (TrueJet, Jet)
zbrelcMig = singleton "/zbrelcmig" <$> physObjH h =$<< zbrelcs
  where
    h =
      hist2DDef
        zbrelcbin
        zbrelcbin
        ("true " <> zbrelcname)
        ("reco " <> zbrelcname)


zbtDiff :: Fills (TrueJet, Jet)
zbtDiff = singleton "/zbtdiff" <$> physObjH h =$<< f
  where
    f (tj, rj) = do
      tz <- zbt tj
      rz <- zbt rj
      return $ tz - rz

    h =
      hist1DDef
        (binD (-1) 50 1)
        ("(true - reco) " <> zbtname)
        (dsigdXpbY zbtname "1")


zbtcDiff :: Fills (TrueJet, Jet)
zbtcDiff = singleton "/zbtcdiff" <$> physObjH h =$<< fmap (uncurry (-)) . zbtcs
  where
    h =
      hist1DDef
        (binD (-1) 50 1)
        ("(true - reco) " <> zbtcname)
        (dsigdXpbY zbtcname "1")


zblDiff :: Fills (TrueJet, Jet)
zblDiff = singleton "/zbldiff" <$> physObjH h =$<< f
  where
    f (tj, rj) = do
      tz <- zbl tj
      rz <- zbl rj
      return $ tz - rz

    h =
      hist1DDef
        (binD (-1) 50 1)
        ("(true - reco) " <> zblname)
        (dsigdXpbY zblname "1")


zblcDiff :: Fills (TrueJet, Jet)
zblcDiff = singleton "/zblcdiff" <$> physObjH h =$<< fmap (uncurry (-)) . zblcs
  where
    h =
      hist1DDef
        (binD (-1) 50 1)
        ("(true - reco) " <> zblcname)
        (dsigdXpbY zblcname "1")

zbrelDiff :: Fills (TrueJet, Jet)
zbrelDiff = singleton "/zbreldiff" <$> physObjH h =$<< f
  where
    f (tj, rj) = do
      tz <- zbrel tj
      rz <- zbrel rj
      return $ tz - rz

    h =
      hist1DDef
        (binD (-1) 50 1)
        ("(true - reco) " <> zbrelname)
        (dsigdXpbY zbrelname "1")

zbrelcDiff :: Fills (TrueJet, Jet)
zbrelcDiff = singleton "/zbrelcdiff" <$> physObjH h =$<< fmap (uncurry (-)) . zbrelcs
  where
    h =
      hist1DDef
        (binD (-1) 50 1)
        ("(true - reco) " <> zbrelcname)
        (dsigdXpbY zbrelcname "1")


nsvtrkDiff :: Fills (TrueJet, Jet)
nsvtrkDiff = singleton "/nsvtrkdiff" <$> physObjH h =$<< fmap (uncurry (-)) . nsvtrks
  where
    h =
      hist1DDef
        (binD (-1) 50 1)
        ("(true - reco) " <> nsvtrkname)
        (dsigdXpbY nsvtrkname "1")

npvtrkDiff :: Fills (TrueJet, Jet)
npvtrkDiff = singleton "/npvtrkdiff" <$> physObjH h =$<< fmap (uncurry (-)) . npvtrks
  where
    h =
      hist1DDef
        (binD (-1) 50 1)
        ("(true - reco) " <> npvtrkname)
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


nsvtrkMig :: Fills (TrueJet, Jet)
nsvtrkMig = singleton "/nsvtrkmig" <$> physObjH h =$<< nsvtrks
  where
    h = hist2DDef nsvtrkbin nsvtrkbin ("true " <> nsvtrkname) ("reco " <> nsvtrkname)


npvtrkMig :: Fills (TrueJet, Jet)
npvtrkMig = singleton "/npvtrkmig" <$> physObjH h =$<< npvtrks
  where
    h = hist2DDef
        npvtrkbin
        npvtrkbin
        ("true " <> npvtrkname)
        ("reco " <> npvtrkname)
