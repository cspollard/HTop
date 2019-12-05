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
{-# LANGUAGE RecordWildCards              #-}

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
import Data.Foldable (fold)
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
  [ hs
  , channelWithLabel "/mu_le_22" (muCut (<= 22)) hs
  , channelWithLabel "/mu_gt_22" (muCut (> 22)) hs
  ]

  where
    go evt = do
      tevt <- view trueEvent evt
      revt <- view recoEvent evt
      return (tevt, revt)

    hs =
      mconcat
      [ recoEventHs =$<< view recoEvent
      , trueEventHs =$<< view trueEvent
      , matchedEventHs =$<< go
      ]

    muCut :: (Double -> Bool) -> Event -> PhysObj Bool
    muCut c Event{..} = do
        re <- _recoEvent
        let m = view mu re
        return $ c m




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
          tjs = trueProbeJets tevt
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
  mconcat
  $ (prefixF "/probejets" <$> bfragHs <$= fmap snd)
    : (prefixF "/truejets" <$> bfragHs <$= fmap fst)
    : [ zbtcMig, zblcMig, zbrelcMig
      , nsvtrkMig, npvtrkMig, msvMig
      , zbtcDiff, zblcDiff, zbrelcDiff
      , nsvtrkDiff, npvtrkDiff
      ]


zbtcMig :: VarFills (TrueJet, Jet)
zbtcMig = singleton "/zbtcmig" <$> h =$<< zbtcs
  where
    h = hist2DDef zbtcbin zbtcbin ("true " <> zbtcname) ("reco " <> zbtcname)


zblcMig :: VarFills (TrueJet, Jet)
zblcMig = singleton "/zblcmig" <$> h =$<< zblcs
  where
    h = hist2DDef zblcbin zblcbin ("true " <> zblcname) ("reco " <> zblcname)


zbrelcMig :: VarFills (TrueJet, Jet)
zbrelcMig = singleton "/zbrelcmig" <$> h =$<< zbrelcs
  where
    h =
      hist2DDef
        zbrelcbin
        zbrelcbin
        ("true " <> zbrelcname)
        ("reco " <> zbrelcname)


npvtrkMig :: VarFills (TrueJet, Jet)
npvtrkMig = singleton "/npvtrkmig" <$> h =$<< npvtrks
  where
    h =
      hist2DDef
        npvtrkbin
        npvtrkbin
        ("true " <> npvtrkname)
        ("reco " <> npvtrkname)


nsvtrkMig :: VarFills (TrueJet, Jet)
nsvtrkMig = singleton "/nsvtrkmig" <$> h =$<< nsvtrks
  where
    h =
      hist2DDef
        nsvtrkbin
        nsvtrkbin
        ("true " <> nsvtrkname)
        ("reco " <> nsvtrkname)


msvMig :: VarFills (TrueJet, Jet)
msvMig = singleton "/msvmig" <$> h =$<< msvs
  where
    h =
      hist2DDef
        msvbin
        msvbin
        ("true " <> msvname)
        ("reco " <> msvname)


zbtcDiff :: VarFills (TrueJet, Jet)
zbtcDiff = singleton "/zbtcdiff" <$> h =$<< fmap (uncurry (-)) . zbtcs
  where
    h =
      hist1DDef
        (binD (-1) 50 1)
        ("(true - reco) " <> zbtcname)
        (dsigdXpbY zbtcname "1")


zblcDiff :: VarFills (TrueJet, Jet)
zblcDiff = singleton "/zblcdiff" <$> h =$<< fmap (uncurry (-)) . zblcs
  where
    h =
      hist1DDef
        (binD (-1) 50 1)
        ("(true - reco) " <> zblcname)
        (dsigdXpbY zblcname "1")


zbrelcDiff :: VarFills (TrueJet, Jet)
zbrelcDiff = singleton "/zbrelcdiff" <$> h =$<< fmap (uncurry (-)) . zbrelcs
  where
    h =
      hist1DDef
        (binD (-1) 50 1)
        ("(true - reco) " <> zbrelcname)
        (dsigdXpbY zbrelcname "1")


nsvtrkDiff :: VarFills (TrueJet, Jet)
nsvtrkDiff = singleton "/nsvtrkdiff" <$> h =$<< fmap (uncurry (-)) . nsvtrks
  where
    h =
      hist1DDef
        (binD (-10) 20 10)
        ("(true - reco) " <> nsvtrkname)
        (dsigdXpbY nsvtrkname "1")


npvtrkDiff :: VarFills (TrueJet, Jet)
npvtrkDiff = singleton "/npvtrkdiff" <$> h =$<< fmap (uncurry (-)) . npvtrks
  where
    h =
      hist1DDef
        (binD (-10) 20 10)
        ("(true - reco) " <> npvtrkname)
        (dsigdXpbY npvtrkname "1")

zbs
  :: (forall a. (HasSVConstits a, HasPVConstits a) => a -> PhysObj Double)
  -> (TrueJet, Jet) -> PhysObj (Double, Double)
zbs f (tj, rj) = do
  tjz <- f tj
  rjz <- f rj
  return (tjz, rjz)


zbtcs, zblcs, zbrelcs, nsvtrks, npvtrks, msvs :: (TrueJet, Jet) -> PhysObj (Double, Double)
zbtcs = zbs zbtc
zblcs = zbs zblc
zbrelcs = zbs zbrelc
nsvtrks = zbs $ fmap (fromIntegral . length) . svChargedConstits
npvtrks = zbs $ fmap (fromIntegral . length) . pvChargedConstits
msvs = zbs $ fmap (view lvM . fold) . pvChargedConstits
