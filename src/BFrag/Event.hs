{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
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
import           Data.Semigroup         (Arg (..), Min (..), Option (..))
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
  channelWithLabel "/matched" filt
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
  fmap (prefixF "/matched")
  $ channelsWithLabels
    [ ("/2precosvtrks", recoSVTrkCut (>= 2))
    , ("/2recosvtrks", recoSVTrkCut (== 2))
    , ("/3recosvtrks", recoSVTrkCut (== 3))
    , ("/4recosvtrks", recoSVTrkCut (== 4))
    , ("/4precosvtrks", recoSVTrkCut (>= 4))
    , ("/5recosvtrks", recoSVTrkCut (== 5))
    , ("/6precosvtrks", recoSVTrkCut (>= 6))
    ]
    . channelsWithLabels
      [ ("/recoptgt30", recoPtCut 30)
      , ("/recoptgt40", recoPtCut 40)
      , ("/recoptgt50", recoPtCut 50)
      , ("/recoptgt75", recoPtCut 75)
      ]
    . mconcat
    $ (prefixF "/probejets" <$> bfragHs <$= fmap snd)
      : (prefixF "/truejets" <$> mappend zbtTrueH bfragHs <$= fmap fst)
      : [ zbtMigration
        , zbtChargedMigration
        , zbtDiff
        , zbtChargedDiff
        , ptSVDiff
        , ptSVChargedDiff
        , nsvMigration
        , npvMigration
        ]

  where
    recoSVTrkCut f = fmap (f . length) . svTracks . snd
    recoPtCut ptMin = pure . (> ptMin) . view lvPt . snd


zbtChargedMigration :: Fills (TrueJet, Jet)
zbtChargedMigration =
  singleton "/recozbtcvstruezbtc"
  <$> physObjH h
  =$<< zbtcs

  where
    h =
      hist2DDef
        (binD 0 21 1.05)
        (binD 0 21 1.05)
        "true charged $z_{p_{\\mathrm T}}$"
        "reco charged $z_{p_{\\mathrm T}}$"


zbtDiff :: Fills (TrueJet, Jet)
zbtDiff =
  singleton "/truezbtminusrecozbtc"
  <$> physObjH h
  =$<< f

  where
    f (tj, rj) = do
      let tz = zBTTrue tj
      rz <- zBTCharged rj
      return $ tz - rz

    h =
      hist1DDef
        (binD (-1) 50 1)
        "$z_{p_{\\mathrm T}}$ (true - charged reco)"
        (dndx "z_{p_{\\mathrm T}}" "1")


zbtChargedDiff :: Fills (TrueJet, Jet)
zbtChargedDiff =
  singleton "/truezbtcminusrecozbtc"
  <$> physObjH h
  =$<< fmap (uncurry (-)) . zbtcs

  where
    h =
      hist1DDef
        (binD (-1) 50 1)
        "charged $z_{p_{\\mathrm T}}$ (true - reco)"
        (dndx "z_{p_{\\mathrm T}}" "1")


ptSVDiff :: Fills (TrueJet, Jet)
ptSVDiff =
  singleton "/trueptsvminusrecoptsvc"
  <$> physObjH h
  =$<< f

  where
    f (tj, rj) = do
      let tpt = view lvPt $ svTrue tj
      rpt <- view lvPt <$> svTrackSum rj
      return $ tpt - rpt

    h =
      hist1DDef
        (binD (-10) 55 100)
        "SV $p_{\\mathrm T}$ (true - charged reco)"
        (dndx pt gev)


ptSVChargedDiff :: Fills (TrueJet, Jet)
ptSVChargedDiff =
  singleton "/trueptsvcminusrecoptsvc"
  <$> physObjH h
  =$<< f

  where
    f (tj, rj) = do
      tpt <- view lvPt <$> svTrackSum tj
      rpt <- view lvPt <$> svTrackSum rj
      return $ tpt - rpt

    h =
      hist1DDef
        (binD (-10) 55 100)
        "charged SV $p_{\\mathrm T}$ (true - reco)"
        (dndx pt gev)


zbtcs :: (TrueJet, Jet) -> PhysObj (Double, Double)
zbtcs (tj, rj) = do
  tjz <- zBTCharged tj
  rjz <- zBTCharged rj
  return (tjz, rjz)


zbtMigration :: Fills (TrueJet, Jet)
zbtMigration =
  singleton "/recozbtcvstruezbt"
  <$> physObjH h
  =$<< f

  where
    f (tj, rj) = do
      let tjz = zBTTrue tj
      rjz <- zBTCharged rj
      return (tjz, rjz)

    h =
      hist2DDef
        (binD 0 21 1.05)
        (binD 0 21 1.05)
        "true $z_{p_{\\mathrm T}}$"
        "reco charged $z_{p_{\\mathrm T}}$"


nsvMigration :: Fills (TrueJet, Jet)
nsvMigration =
  singleton "/reconsvtrksvstruensvtrks"
  <$> physObjH h
  =$<< f

  where
    f (tj, rj) = do
       tnt <- fromIntegral <$> nSVTracks tj
       rnt <- fromIntegral <$> nSVTracks rj
       return (tnt, rnt)

    h =
      hist2DDef
        (binD 0 10 10)
        (binD 0 10 10)
        "true $n$ SV tracks"
        "reco $n$ SV tracks"


npvMigration :: Fills (TrueJet, Jet)
npvMigration =
  singleton "/reconpvtrksvstruenpvtrks"
  <$> physObjH h
  =$<< f

  where
    f (tj, rj) = do
       tnt <- fromIntegral <$> nPVTracks tj
       rnt <- fromIntegral <$> nPVTracks rj
       return (tnt, rnt)

    h =
      hist2DDef
        (binD 0 20 20)
        (binD 0 20 20)
        "true $n$ PV tracks"
        "reco $n$ PV tracks"
