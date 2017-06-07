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
matchedEventHs = F.handles folded matchedJetHs <$= fmap join . sequenceL . fmap go
  where
    go :: (TrueEvent, RecoEvent) -> [PhysObj (TrueJet, Jet)]
    go (tevt, revt) =
      let rjs = probeJets revt
          tjs = toListOf (trueJets . traverse . filtered tfilt) tevt
          matches = (fromMaybe' =<<) . fmap (fmap swap . sequence . trueMatch tjs) <$> rjs
      in matches

    tfilt j = lengthOf (tjBHadrons.traverse) j == 1 && view lvPt j > 25
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
  channelsWithLabels
    [ ("/2psvtrks", pure . (>= 2) . length . svTracks . snd)
    , ("/2svtrks", pure . (== 2) . length . svTracks . snd)
    , ("/3svtrks", pure . (== 3) . length . svTracks . snd)
    , ("/4svtrks", pure . (== 4) . length . svTracks . snd)
    , ("/5svtrks", pure . (== 5) . length . svTracks . snd)
    , ("/4psvtrks", pure . (>= 4) . length . svTracks . snd)
    , ("/6psvtrks", pure . (>= 6) . length . svTracks . snd)
    ]
  . channelsWithLabels
    [ ("/ptgt30", pure . (> 30) . view lvPt . snd)
    , ("/ptgt40", pure . (> 40) . view lvPt . snd)
    , ("/ptgt50", pure . (> 50) . view lvPt . snd)
    , ("/ptgt75", pure . (> 75) . view lvPt . snd)
    ]
  . mconcat
  $ (prefixF "/matchedJets" <$> bfragHs <$= fmap snd)
    : (prefixF "/matchedTruthJets" <$> bfragHs <$= fmap fst)
    : [zbtMigration, nsvMigration, npvMigration]


zbtMigration :: Fills (TrueJet, Jet)
zbtMigration =
  singleton "/recozbtvstruezbt"
  <$> physObjH h
  <$= fmap (bimap zBT zBT)

  where
    h =
      hist2DDef
        (binD 0 7 1.05)
        (binD 0 21 1.05)
        "true $z_{p_{\\mathrm T}}$"
        "reco $z_{p_{\\mathrm T}}$"


nsvMigration :: Fills (TrueJet, Jet)
nsvMigration =
  singleton "/reconsvtrksvstruensvtrks"
  <$> physObjH h
  <$= fmap (bimap (fromIntegral . nSVTracks) (fromIntegral . nSVTracks))

  where
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
  <$= fmap (bimap (fromIntegral . nPVTracks) (fromIntegral . nPVTracks))

  where
    h =
      hist2DDef
        (binD 0 20 20)
        (binD 0 20 20)
        "true $n$ PV tracks"
        "reco $n$ PV tracks"
