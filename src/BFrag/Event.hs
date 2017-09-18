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
import           Data.Foldable          (fold)
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
      : [ zbtMig
        , zbtcMig
        , zbtDiff
        , zbtcDiff
        , svPtDiff
        , svPtcDiff
        , pvPtDiff
        , pvPtcDiff
        , nsvMig
        , npvMig
        ]

  -- where
  --   recoSVTrkCut f = fmap (f . length) . svChargedConstits . snd
  --   recoPtCut ptMin = pure . (> ptMin) . view lvPt . snd


zbtMig :: Fills (TrueJet, Jet)
zbtMig =
  singleton "/zbtmig"
  <$> physObjH h
  =$<< zbts

  where
    h =
      hist2DDef
        (binD 0 21 1.05)
        (binD 0 21 1.05)
        "true charged $z_{p_{\\mathrm T}}$"
        "reco charged $z_{p_{\\mathrm T}}$"

zbtcMig :: Fills (TrueJet, Jet)
zbtcMig = singleton "/zbtcmig" <$> physObjH h =$<< zbtcs
  where
    h =
      hist2DDef
        (binD 0 21 1.05)
        (binD 0 21 1.05)
        "true charged $z_{p_{\\mathrm T}}$"
        "reco charged $z_{p_{\\mathrm T}}$"


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
        "$z_{p_{\\mathrm T}}$ (true - charged reco)"
        (dndx "z_{p_{\\mathrm T}}" "1")


zbtcDiff :: Fills (TrueJet, Jet)
zbtcDiff = singleton "/zbtcdiff" <$> physObjH h =$<< fmap (uncurry (-)) . zbtcs
  where
    h =
      hist1DDef
        (binD (-1) 50 1)
        "charged $z_{p_{\\mathrm T}}$ (true - reco)"
        (dndx "z_{p_{\\mathrm T}}" "1")


pvPtDiff :: Fills (TrueJet, Jet)
pvPtDiff = singleton "/pvptdiff" <$> physObjH h =$<< f
  where
    f (tj, rj) = do
      tpt <- view lvPt . fold <$> pvConstits tj
      rpt <- view lvPt . fold <$> pvConstits rj
      return $ tpt - rpt

    h =
      hist1DDef
        (binD (-10) 55 100)
        "PV $p_{\\mathrm T}$ (true - charged reco)"
        (dndx pt gev)


pvPtcDiff :: Fills (TrueJet, Jet)
pvPtcDiff = singleton "/pvptcdiff" <$> physObjH h =$<< f
  where
    f (tj, rj) = do
      tpt <- view lvPt . fold <$> pvChargedConstits tj
      rpt <- view lvPt . fold <$> pvChargedConstits rj
      return $ tpt - rpt

    h =
      hist1DDef
        (binD (-10) 55 100)
        "charged PV $p_{\\mathrm T}$ (true - reco)"
        (dndx pt gev)

svPtDiff :: Fills (TrueJet, Jet)
svPtDiff = singleton "/svptdiff" <$> physObjH h =$<< f
  where
    f (tj, rj) = do
      tpt <- view lvPt . fold <$> svConstits tj
      rpt <- view lvPt . fold <$> svConstits rj
      return $ tpt - rpt

    h =
      hist1DDef
        (binD (-10) 55 100)
        "SV $p_{\\mathrm T}$ (true - charged reco)"
        (dndx pt gev)


svPtcDiff :: Fills (TrueJet, Jet)
svPtcDiff = singleton "/svptcdiff" <$> physObjH h =$<< f
  where
    f (tj, rj) = do
      tpt <- view lvPt . fold <$> svChargedConstits tj
      rpt <- view lvPt . fold <$> svChargedConstits rj
      return $ tpt - rpt

    h =
      hist1DDef
        (binD (-10) 55 100)
        "charged SV $p_{\\mathrm T}$ (true - reco)"
        (dndx pt gev)


zbts :: (TrueJet, Jet) -> PhysObj (Double, Double)
zbts (tj, rj) = do
  tjz <- zbt tj
  rjz <- zbt rj
  return (tjz, rjz)


zbtcs :: (TrueJet, Jet) -> PhysObj (Double, Double)
zbtcs (tj, rj) = do
  tjz <- zbtc tj
  rjz <- zbtc rj
  return (tjz, rjz)


nsvMig :: Fills (TrueJet, Jet)
nsvMig = singleton "/nsvmig" <$> physObjH h =$<< f
  where
    f (tj, rj) = do
       tnt <- fromIntegral . length <$> svChargedConstits tj
       rnt <- fromIntegral . length <$> svChargedConstits rj
       return (tnt, rnt)

    h =
      hist2DDef
        (binD 0 10 10)
        (binD 0 10 10)
        "true $n$ SV tracks"
        "reco $n$ SV tracks"


npvMig :: Fills (TrueJet, Jet)
npvMig = singleton "/npvmig" <$> physObjH h =$<< f
  where
    f (tj, rj) = do
       tnt <- fromIntegral . length <$> pvChargedConstits tj
       rnt <- fromIntegral . length <$> pvChargedConstits rj
       return (tnt, rnt)

    h =
      hist2DDef
        (binD 0 20 20)
        (binD 0 20 20)
        "true $n$ PV tracks"
        "reco $n$ PV tracks"
