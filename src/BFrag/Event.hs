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
import           Control.Monad          (join, guard)
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
  channel "/elmujjmatched"
  $ foldlMoore matchedJetHs
    <$= fmap join . collapsePO . fmap go
    =$<< filt

  where
    filt :: (TrueEvent, RecoEvent) -> PhysObj (TrueEvent, RecoEvent)
    filt (tevt, revt) = do
      revt' <- elmujj revt
      guard $ elmujjTrue tevt
      return (tevt, revt')


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


matchedJetHs :: Fills (TrueJet, Jet)
matchedJetHs =
  (channel "/probejets" $ bfragHs <$= fmap snd)
  <> (channel "/truejets" $ bfragHs <$= fmap fst)
  <> mconcat
      [ zbtMig, zbtcMig, zblMig, zblcMig, zbrelMig, zbrelcMig
      -- , nsvtrkMig, npvtrkMig
      -- , zbtDiff, zbtcDiff, zblDiff, zblcDiff, zbrelDiff, zbrelcDiff
      -- , nsvtrkDiff, npvtrkDiff, nsvtrkRelDiff, npvtrkRelDiff
      -- , svPtDiff
      -- , svPtcDiff
      -- , pvPtDiff
      -- , pvPtcDiff
      ]


zbtMig :: Fills (TrueJet, Jet)
zbtMig = h =$<< zbts
  where
    h = histo2DDef zbtbin zbtbin ("true " <> zbtname) ("reco " <> zbtname) "/zbtmig"


zbtcMig :: Fills (TrueJet, Jet)
zbtcMig = h =$<< zbtcs
  where
    h = histo2DDef zbtcbin zbtcbin ("true " <> zbtcname) ("reco " <> zbtcname) "/zbtcmig"


zblMig :: Fills (TrueJet, Jet)
zblMig = h =$<< zbls
  where
    h = histo2DDef zblbin zblbin ("true " <> zblname) ("reco " <> zblname) "/zblmig"


zblcMig :: Fills (TrueJet, Jet)
zblcMig = h =$<< zblcs
  where
    h = histo2DDef zblcbin zblcbin ("true " <> zblcname) ("reco " <> zblcname) "/zblcmig"


zbrelMig :: Fills (TrueJet, Jet)
zbrelMig = h =$<< zbrels
  where
    h = histo2DDef zbrelbin zbrelbin ("true " <> zbrelname) ("reco " <> zbrelname) "/zbrelmig"


zbrelcMig :: Fills (TrueJet, Jet)
zbrelcMig = h =$<< zbrelcs
  where
    h = histo2DDef zbrelcbin zbrelcbin ("true " <> zbrelcname) ("reco " <> zbrelcname) "/zbrelcmig"


-- zbtDiff :: Fills (TrueJet, Jet)
-- zbtDiff = singleton "/zbtdiff" <$> h =$<< f
--   where
--     f (tj, rj) = do
--       tz <- zbt tj
--       rz <- zbt rj
--       return $ tz - rz

--     h =
--       histo1DDef
--         (evenBins' (-1) 50 1)
--         ("(true - reco) " <> zbtname)
--         (dsigdXpbY zbtname "1")


-- zbtcDiff :: Fills (TrueJet, Jet)
-- zbtcDiff = singleton "/zbtcdiff" <$> h =$<< fmap (uncurry (-)) . zbtcs
--   where
--     h =
--       histo1DDef
--         (evenBins' (-1) 50 1)
--         ("(true - reco) " <> zbtcname)
--         (dsigdXpbY zbtcname "1")


-- zblDiff :: Fills (TrueJet, Jet)
-- zblDiff = singleton "/zbldiff" <$> h =$<< f
--   where
--     f (tj, rj) = do
--       tz <- zbl tj
--       rz <- zbl rj
--       return $ tz - rz

--     h =
--       histo1DDef
--         (evenBins' (-1) 50 1)
--         ("(true - reco) " <> zblname)
--         (dsigdXpbY zblname "1")


-- zblcDiff :: Fills (TrueJet, Jet)
-- zblcDiff = singleton "/zblcdiff" <$> h =$<< fmap (uncurry (-)) . zblcs
--   where
--     h =
--       histo1DDef
--         (evenBins' (-1) 50 1)
--         ("(true - reco) " <> zblcname)
--         (dsigdXpbY zblcname "1")


-- zbrelDiff :: Fills (TrueJet, Jet)
-- zbrelDiff = singleton "/zbreldiff" <$> h =$<< f
--   where
--     f (tj, rj) = do
--       tz <- zbrel tj
--       rz <- zbrel rj
--       return $ tz - rz

--     h =
--       histo1DDef
--         (evenBins' (-1) 50 1)
--         ("(true - reco) " <> zbrelname)
--         (dsigdXpbY zbrelname "1")


-- zbrelcDiff :: Fills (TrueJet, Jet)
-- zbrelcDiff = singleton "/zbrelcdiff" <$> h =$<< fmap (uncurry (-)) . zbrelcs
--   where
--     h =
--       histo1DDef
--         (evenBins' (-1) 50 1)
--         ("(true - reco) " <> zbrelcname)
--         (dsigdXpbY zbrelcname "1")


-- nsvtrkDiff :: Fills (TrueJet, Jet)
-- nsvtrkDiff = singleton "/nsvtrkdiff" <$> h =$<< fmap (uncurry (-)) . nsvtrks
--   where
--     h =
--       histo1DDef
--         (evenBins' (-10) 20 10)
--         ("(true - reco) " <> nsvtrkname)
--         (dsigdXpbY nsvtrkname "1")


-- npvtrkDiff :: Fills (TrueJet, Jet)
-- npvtrkDiff = singleton "/npvtrkdiff" <$> h =$<< fmap (uncurry (-)) . npvtrks
--   where
--     h =
--       histo1DDef
--         (evenBins' (-10) 20 10)
--         ("(true - reco) " <> npvtrkname)
--         (dsigdXpbY npvtrkname "1")


-- nsvtrkRelDiff :: Fills (TrueJet, Jet)
-- nsvtrkRelDiff = singleton "/nsvtrkreldiff" <$> (h =$<< f)
--   where
--     f :: (TrueJet, Jet) -> PhysObj Double
--     f (tj, j) = do
--       ntj <- fromIntegral . length <$> svChargedConstits tj
--       nj <- fromIntegral . length <$> svChargedConstits j
--       return $ (ntj - nj) / ntj

--     h =
--       histo1DDef
--         (evenBins' (-1) 20 1)
--         ("(true - reco) / true " <> nsvtrkname)
--         (dsigdXpbY nsvtrkname "1")


-- npvtrkRelDiff :: Fills (TrueJet, Jet)
-- npvtrkRelDiff = singleton "/npvtrkreldiff" <$> h =$<< f
--   where
--     f (tj, j) = do
--       ntj <- fromIntegral . length <$> pvChargedConstits tj
--       nj <- fromIntegral . length <$> pvChargedConstits j
--       return $ (ntj - nj) / ntj

--     h =
--       histo1DDef
--         (evenBins' (-1) 20 1)
--         ("(true - reco) / true " <> npvtrkname)
--         (dsigdXpbY npvtrkname "1")


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


-- nsvtrkMig :: Fills (TrueJet, Jet)
-- nsvtrkMig = singleton "/nsvtrkmig" <$> h =$<< nsvtrks
--   where
--     h = histo2DDef nsvtrkbin nsvtrkbin ("true " <> nsvtrkname) ("reco " <> nsvtrkname)


-- npvtrkMig :: Fills (TrueJet, Jet)
-- npvtrkMig = singleton "/npvtrkmig" <$> h =$<< npvtrks
--   where
--     h = histo2DDef
--         npvtrkbin
--         npvtrkbin
--         ("true " <> npvtrkname)
--         ("reco " <> npvtrkname)
