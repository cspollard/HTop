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
import           Data.Maybe             (catMaybes)
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
matchedEventHs = F.handles folded matchedJetHs <$= sequenceL . fmap go
  where
    go (tevt, revt) =
      let tjs = view trueJets tevt
          rjs = view jets revt
          matches = catMaybes $ sequence . trueMatch tjs <$> rjs
      in matches


trueMatch :: [TrueJet] -> Jet -> (Jet, Maybe TrueJet)
trueMatch tjs j = (j,) . getOption $ do
  Min (Arg dr tj) <-
    foldMap (\tj' -> Option . Just . Min $ Arg (lvDREta j tj') tj') tjs
  if dr < 0.3
    then return tj
    else Option Nothing


matchedJetHs :: Fills (Jet, TrueJet)
matchedJetHs = mconcat [zbtMigration, nsvMigration, npvMigration]


zbtMigration :: Fills (Jet, TrueJet)
zbtMigration =
  singleton "/recozbtvstruezbt"
  <$> physObjH h
  <$= fmap (swap . bimap zBT zBT)

  where
    h =
      hist2DDef
        (binD 0 7 1.05)
        (binD 0 21 1.05)
        "true $z_{p_{\\mathrm T}}$"
        "reco $z_{p_{\\mathrm T}}$"


nsvMigration :: Fills (Jet, TrueJet)
nsvMigration =
  singleton "/reconsvtrksvstruensvtrks"
  <$> physObjH h
  <$= fmap (swap . bimap (fromIntegral . nSVTracks) (fromIntegral . nSVTracks))

  where
    h =
      hist2DDef
        (binD 0 10 10)
        (binD 0 10 10)
        "true $n$ SV tracks"
        "reco $n$ SV tracks"


npvMigration :: Fills (Jet, TrueJet)
npvMigration =
  singleton "/reconpvtrksvstruenpvtrks"
  <$> physObjH h
  <$= fmap (swap . bimap (fromIntegral . nPVTracks) (fromIntegral . nPVTracks))

  where
    h =
      hist2DDef
        (binD 0 20 20)
        (binD 0 20 20)
        "true $n$ PV tracks"
        "reco $n$ PV tracks"


-- matchjetHs :: Fills (Jet, Maybe TrueJet)
-- matchjetHs =
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
--
