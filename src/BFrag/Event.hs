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
  , readRunNumber, readEventNumber, readReco
  , recoEvent, trueEvent, recoVariations
  ) where

import           Atlas
import           BFrag.BFrag            as X
import           BFrag.Electron         as X
import           BFrag.Jet              as X
import           BFrag.Muon             as X
import           BFrag.PtEtaPhiE        as X
import           BFrag.RecoEvent        as X
import           BFrag.TrueEvent        as X
import           Control.Lens
import           Data.These
-- import           Control.Monad.Trans    (lift)
import           Data.HEP.LorentzVector as X
import qualified Data.Text              as T
import           Data.TTree
import           GHC.Generics           (Generic)
import           Pipes

data Event =
  Event
    { _runNumber   :: CUInt
    , _eventNumber :: CULong
    , _events      :: These (PhysObj TrueEvent) (PhysObj RecoEvent)
    } deriving (Generic)


events :: Lens' Event (These (PhysObj TrueEvent) (PhysObj RecoEvent))
events = lens _events $ \e x -> e { _events = x }

trueEvent :: Traversal' Event (PhysObj TrueEvent)
trueEvent = events . here

recoEvent :: Traversal' Event (PhysObj RecoEvent)
recoEvent = events . there


readRunNumber :: (MonadIO m, MonadFail m) => TreeRead m CUInt
readRunNumber = do
  x <- readBranch "runNumber"
  liftIO $ print x
  return x

readEventNumber :: (MonadIO m, MonadFail m) => TreeRead m CULong
readEventNumber = do
  x <- readBranch "eventNumber"
  liftIO $ print x
  return x

recoVariations
  :: Monad m
  => StrictMap T.Text (Producer ((CUInt, CULong), PhysObj RecoEvent) m ())
  -> Producer (StrictMap T.Text (Maybe ((CUInt, CULong), PhysObj RecoEvent))) m ()
recoVariations = alignPipesBy fst


readReco
  :: (MonadFail m, MonadIO m)
  => DataMC' -> TreeRead m ((CUInt, CULong), PhysObj RecoEvent)
readReco dmc =
  (,)
  <$>
    ( (,) <$> readRunNumber <*> readEventNumber )
  <*> readRecoEvent dmc

--
-- eventHs :: Fills Event
-- eventHs =
--   mconcat
--     [ F.handles (recoEvent._Just) recoEventHs
--     , F.handles _Just trueEventHs
--       <$= view trueEvent
--     -- TODO
--     -- probeJetHs
--     ]

  -- TODO
  -- TODO
  -- channelWithLabel "/elmujj" elmujj




runNumber :: Lens' Event CUInt
runNumber = lens _runNumber $ \e x -> e { _runNumber = x }

eventNumber :: Lens' Event CULong
eventNumber = lens _eventNumber $ \e x -> e { _eventNumber = x }


-- zbtMigration :: Fills (Jet, TrueJet)
-- zbtMigration =
--   singleton "/recozbtvstruezbt"
--   <$> h
--   <$= swap . bimap zBT zBT
--
--   where
--     h =
--       hist2DDef
--         (binD 0 7 1.05)
--         (binD 0 21 1.05)
--         "true $z_{p_{\\mathrm T}}$"
--         "reco $z_{p_{\\mathrm T}}$"
--
-- nsvMigration :: Fills (Jet, TrueJet)
-- nsvMigration =
--   singleton "/reconsvtrksvstruensvtrks"
--   <$> h
--   <$= swap . bimap (fromIntegral . nSVTracks) (fromIntegral . nSVTracks)
--
--   where
--     h =
--       hist2DDef
--         (binD 0 10 10)
--         (binD 0 10 10)
--         "true $n$ SV tracks"
--         "reco $n$ SV tracks"
--
-- npvMigration :: Fills (Jet, TrueJet)
-- npvMigration =
--   singleton "/reconpvtrksvstruenpvtrks"
--   <$> h
--   <$= swap . bimap (fromIntegral . nPVTracks) (fromIntegral . nPVTracks)
--
--   where
--     h =
--       hist2DDef
--         (binD 0 20 20)
--         (binD 0 20 20)
--         "true $n$ PV tracks"
--         "reco $n$ PV tracks"
--
-- recoVsTrueHs :: Fills (Jet, TrueJet)
-- recoVsTrueHs = mconcat [zbtMigration, nsvMigration, npvMigration]
--
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
-- trueMatch :: [TrueJet] -> Jet -> (Jet, Maybe TrueJet)
-- trueMatch tjs j = (j,) . getOption $ do
--   Min (Arg dr tj) <-
--     foldMap (\tj' -> Option . Just . Min $ Arg (lvDREta j tj') tj') tjs
--   if dr < 0.3
--     then return tj
--     else Option Nothing
