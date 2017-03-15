{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeFamilies              #-}

module Data.Atlas.Event
  ( Event(..)
  , module X
  , runNumber, eventNumber, mu
  , electrons, muons, jets, met
  , truthjets
  , eventHs, overlapRemoval
  , readEvent, elmujj, pruneJets
  , eventWeight, probeJets
  , recoVsTruthHs, truthMatchedProbeJets
  , withWeight
  ) where

import qualified Control.Foldl            as F
import           Control.Lens
import           Data.Atlas.Corrected
import           Data.HEP.LorentzVector   as X
import           Data.Maybe               (catMaybes, maybeToList)
import           Data.Monoid
import           Data.TTree
import           Data.Tuple               (swap)
import           GHC.Float
import           GHC.Generics             (Generic)

import           Data.Atlas.BFrag         as X
import           Data.Atlas.Electron      as X
import           Data.Atlas.Histogramming
import           Data.Atlas.Jet           as X
import           Data.Atlas.Muon          as X
import           Data.Atlas.PtEtaPhiE     as X
import           Data.Atlas.Systematics   as X
import           Data.Atlas.TruthJet      as X


data Event =
  Event
    { _runNumber   :: Int
    , _eventNumber :: Int
    , _eventWeight :: Vars SF
    , _mu          :: Double
    , _electrons   :: [Electron]
    , _muons       :: [Muon]
    , _jets        :: [Jet]
    , _met         :: PtEtaPhiE
    , _truthjets   :: Maybe [TruthJet]
    } deriving (Generic, Show)


withWeight :: Event -> Corrected (Vars SF) Event
withWeight evt =
  let wgts = view eventWeight evt
  in withCorrection (evt, wgts)

readMET :: MonadIO m => String -> String -> TR m PtEtaPhiE
readMET m p = do
  et <- float2Double <$> readBranch m
  phi <- float2Double <$> readBranch p
  return $ PtEtaPhiE et 0 phi et


readEvent :: MonadIO m => Bool -> TR m Event
readEvent isData =
    Event
      <$> fmap ci2i (readBranch "Run")
      <*> fmap ci2i (readBranch "Event")
      <*> evtWgt isData
      <*> fmap float2Double (readBranch "Mu")
      <*> readElectrons
      <*> readMuons
      <*> readJets isData
      <*> readMET "ETMiss" "ETMissPhi"
      <*> if isData then return Nothing else Just <$> readTruthJets
    where
      ci2i :: CInt -> Int
      ci2i = fromEnum


muH :: Foldl (Corrected SF Event) (Vars (Folder YodaObj))
muH =
  sequenceA . singleton "/mu"
  <$> hist1DDef (binD 0 25 100) "$< \\mu >$" (dsigdXpbY "\\mu" "1")
  <$= view mu

metH :: Foldl (Corrected SF Event) (Vars (Folder YodaObj))
metH =
  fmap (prefixF "/met" . over (traverse.xlabel) ("$E_{\\rm T}^{\\rm miss}$ " <>))
  <$> ptH
  <$= view met


recoVsTruthHs :: Foldl (Corrected SF (Jet, TruthJet)) (Vars (Folder YodaObj))
recoVsTruthHs =
  sequenceA . singleton "/recobfragvstruebfrag"
  <$> h
  <$= swap . bimap zBT zBT

  where
    h =
      hist2DDef
        (binD 0 22 1.1)
        (binD 0 22 1.1)
        "true $z_{p_{\\mathrm T}}$"
        "reco $z_{p_{\\mathrm T}}$"


truthMatchedProbeJets
  :: Event -> [Corrected (Vars SF) (Jet, TruthJet)]
truthMatchedProbeJets e =
  let tjs = concat . maybeToList $ view truthjets e
      js = probeJets e
      ms = (fmap.fmap) (`matchJTJ` tjs) js
  in catMaybes $ fmap sequence ms


runCorrectedVars :: Corrected (Vars b) a -> Vars (Corrected b a)
runCorrectedVars = fmap withCorrection . sequenceA . runCorrected

eventHs :: Foldl Event (Vars (Folder YodaObj))
eventHs =
  bindF (runCorrectedVars . withWeight)
  . channelWithLabel "/elmujj" (elmujj . fst . runCorrected)
  $ mconcat
    [ muH
    , metH
    , fmap (prefixF "/jets" . over (traverse.xlabel) ("jet " <>))
      <$> F.handles (to sequence.folded) lvHs <$= view jets
    , fmap (prefixF "/electrons" . over (traverse.xlabel) ("electron " <>))
      <$> F.handles (to sequence.folded) electronHs <$= view electrons
    , fmap (prefixF "/muons" . over (traverse.xlabel) ("muon " <>))
      <$> F.handles (to sequence.folded) muonHs <$= view muons
    -- , probeJetHs
    ]

-- probeJetHs :: F.Fold Event (Vars YodaFolder)
-- probeJetHs =
--   F.premap probeJets . F.handles folded . withVariedSFs
--     $ prefixYF "/probejets" . over (traverse.xlabel) ("probe jet " <>)
--       <$> jetHs

  -- , prefixYF "/truthbjets" . over (traverse.xlabel) ("truth $b$-jet " <>)
  --   <$> F.handles (to sequence.folded) truthJetHs
  --   <$= ( concat . maybeToList . view truthjets )
  -- , fmap (prefixYF "/truthrecojets")
  --   $ recoVsTruthHs <$$$= truthMatchedProbeJets



runNumber :: Lens' Event Int
runNumber = lens _runNumber $ \e x -> e { _runNumber = x }

eventNumber :: Lens' Event Int
eventNumber = lens _eventNumber $ \e x -> e { _eventNumber = x }

eventWeight :: Lens' Event (Vars SF)
eventWeight = lens _eventWeight $ \e x -> e { _eventWeight = x }

mu :: Lens' Event Double
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

probeJets :: Event -> [Corrected (Vars SF) Jet]
probeJets evt =
  case view jets evt of
    [j1, j2] -> probeJet j1 j2 ++ probeJet j2 j1
    _        -> []
  where
    probeJet j j' = sequenceA $ do
      bt <- view isBTagged j
      if bt && hasSV j'
        then return [j']
        else return []


elmujj :: Event -> Bool
elmujj e =
  let els = _electrons e
      mus = _muons e
      js = _jets e
  in length els == 1
      && length mus == 1
      && length js == 2
      && lvDREta (head js) (head $ tail js) > 1.0


overlapRemoval :: Event -> Event
overlapRemoval evt = over jets (filter filt) evt
  where
    leps = view electrons evt
    filt = not . any (< 0.2) . traverse lvDREta leps


pruneJets :: Event -> Event
pruneJets =
  over jets
  $ filter (\j -> view lvPt j > 30 && view lvAbsEta j < 2.1)
