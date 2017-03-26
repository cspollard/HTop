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
  , runNumber, eventNumber, mu
  , electrons, muons, jets, met
  , truthjets
  , eventHs
  , readEvent, elmujj, probeJets
  ) where

import qualified Control.Foldl          as F
import           Control.Lens
import           Control.Monad          (join)
import           Data.HEP.LorentzVector as X
import           Data.Maybe             (fromMaybe)
import           Data.Monoid
import           Data.TTree
import           GHC.Float
import           GHC.Generics           (Generic)

import           Atlas
import           BFrag.BFrag            as X
import           BFrag.Electron         as X
import           BFrag.Jet              as X
import           BFrag.Muon             as X
import           BFrag.PtEtaPhiE        as X
import           BFrag.Systematics      as X
import           BFrag.TruthJet         as X


data Event =
  Event
    { _runNumber   :: Int
    , _eventNumber :: Int
    , _mu          :: Vars Double
    , _nPV         :: Double
    , _electrons   :: [Electron]
    , _muons       :: [Muon]
    , _jets        :: [Jet]
    , _met         :: PtEtaPhiE
    , _truthjets   :: Maybe [TruthJet]
    } deriving (Generic, Show)

readMET :: MonadIO m => String -> String -> TR m PtEtaPhiE
readMET m p = do
  et <- float2Double <$> readBranch m
  phi <- float2Double <$> readBranch p
  return $ PtEtaPhiE et 0 phi et

muVars :: DataMC' -> Double -> Vars Double
muVars Data' m =
    -- TODO
    -- here we assume the scaling has already taken place...
    pure m & ix "datapileupup" .~ m*1.09 & ix "datapileupdown" .~ m/1.09
muVars _ m = pure m

readEvent :: MonadIO m => DataMC' -> TR m (PhysObj Event)
readEvent dmc = do
  wgt <- evtWgt dmc
  evt <-
    Event
    <$> fmap ci2i (readBranch "Run")
    <*> fmap ci2i (readBranch "Event")
    <*> (muVars dmc . float2Double <$> readBranch "Mu")
    <*> (float2Double <$> readBranch "NPVtx")
    <*> readElectrons
    <*> readMuons
    <*> readJets dmc
    <*> readMET "ETMiss" "ETMissPhi"
    <*> case dmc of
          Data' -> return Nothing
          MC' _ -> Just <$> readTruthJets

  return $ onlySFVars wgt evt

  where
    ci2i :: CInt -> Int
    ci2i = fromEnum


muH :: Fills Event
muH =
  fmap (singleton "/mu")
  . innerF (onlyObjVars . view mu)
  $ hist1DDef (binD 0 25 100) "$< \\mu >$" (dsigdXpbY "<\\mu>" "1")

npvH :: Fills Event
npvH =
  fmap (singleton "/npv")
  . innerF (onlyObjVars . view mu)
  $ hist1DDef (binD 0 25 50) "$< \\mu >$" (dsigdXpbY "npv" "1")

metH :: Fills Event
metH =
  prefixF "/met"
  . over (traverse.traverse.xlabel) ("$E_{\\rm T}^{\\rm miss}$ " <>)
  <$> ptH
  <$= view met


eventHs :: Fills Event
eventHs =
  channelWithLabel "/elmujj" elmujj
  $ mconcat
    [ muH
    , npvH
    , metH
    , prefixF "/jets" . over (traverse.traverse.xlabel) ("jet " <>)
      <$> F.handles (to sequence.folded) lvHs
      <$= view jets
    , prefixF "/electrons" . over (traverse.traverse.xlabel) ("electron " <>)
      <$> F.handles (to sequence.folded) electronHs
      <$= view electrons
    , prefixF "/muons" . over (traverse.traverse.xlabel) ("muon " <>)
      <$> F.handles (to sequence.folded) muonHs
      <$= view muons
    , prefixF "/truthjets" . over (traverse.traverse.xlabel) ("truth jet " <>)
      <$> F.handles (to sequence._Just.to sequence.folded) truthjetHs
      <$= view truthjets
    , probeJetHs
    ]

-- TODO
-- can I make this into a traversal instead of a list?
probeJets :: Event -> [PhysObj Jet]
probeJets evt =
  case view jets evt of
    [j1, j2] ->
      if lvDREta j1 j2 > 1.0
        then probeJet j1 j2 ++ probeJet j2 j1
        else []
    _        -> []
  where
    probeJet j j' = sequenceA $ do
      bt <- view isBTagged j
      if bt && hasSV j' && (view lvAbsEta j' < 2.1)
        then return [j']
        else return []

probeJetHs :: Fills Event
probeJetHs = mconcat
  [ prefixF "/probejets" . over (traverse.traverse.xlabel) ("probe jet " <>)
    <$> F.premap (fmap join . sequenceA) (F.handles folded jetHs)
    <$= (\e -> fmap (truthMatch . fromMaybe [] $ view truthjets e) <$> probeJets e)
  ]




runNumber :: Lens' Event Int
runNumber = lens _runNumber $ \e x -> e { _runNumber = x }

eventNumber :: Lens' Event Int
eventNumber = lens _eventNumber $ \e x -> e { _eventNumber = x }

mu :: Lens' Event (Vars Double)
mu = lens _mu $ \e x -> e { _mu = x }

electrons :: Lens' Event [Electron]
electrons = lens _electrons $ \e x -> e { _electrons = x }

muons :: Lens' Event [Muon]
muons = lens _muons $ \e x -> e { _muons = x }

jets :: Lens' Event [Jet]
jets = lens _jets $ \e x -> e { _jets = x }

met :: Lens' Event PtEtaPhiE
met = lens _met $ \e x -> e { _met = x }

truthjets :: Lens' Event (Maybe [TruthJet])
truthjets = lens _truthjets $ \e x -> e { _truthjets = x }


elmujj :: Event -> PhysObj Bool
elmujj e =
  let els = _electrons e
      mus = _muons e
      js = _jets e
  in return
    $ length els == 1
      && length mus == 1
      && case js of
        [j1, j2] -> lvDREta j1 j2 > 1.0
        _        -> False
