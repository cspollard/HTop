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
  , SystMap, withWeights
  , readEvent, elmujj, pruneJets
  ) where

import qualified Control.Foldl            as F
import           Control.Lens
import           Data.Atlas.Corrected
import           Data.HEP.LorentzVector   as X
import qualified Data.Map.Strict          as M
import           Data.Maybe               (catMaybes, maybeToList)
import           Data.Monoid
import qualified Data.Text                as T
import           Data.TTree
import           Data.Tuple               (swap)
import           GHC.Float
import           GHC.Generics             (Generic)

import           Data.Atlas.Electron      as X
import           Data.Atlas.Histogramming
import           Data.Atlas.Jet           as X
import           Data.Atlas.Muon          as X
import           Data.Atlas.PtEtaPhiE     as X
import           Data.Atlas.TruthJet      as X


data Event =
  Event
    { _runNumber   :: Int
    , _eventNumber :: Int
    , _mu          :: Double
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


readEvent :: MonadIO m => Bool -> TR m Event
readEvent isData =
    Event
      <$> fmap ci2i (readBranch "Run")
      <*> fmap ci2i (readBranch "Event")
      <*> fmap float2Double (readBranch "Mu")
      <*> readElectrons
      <*> readMuons
      <*> readJets isData
      <*> readMET "ETMiss" "ETMissPhi"
      <*> if isData then return Nothing else Just <$> readTruthJets
    where
      ci2i :: CInt -> Int
      ci2i = fromEnum


muH :: Fill Event
muH =
  hist1DDef
    (binD 0 25 100)
    "$< \\mu >$"
    (dsigdXpbY "\\mu" "1")
    "/mu"
    <$= view mu


recoVsTruthHs :: Fill (Jet, TruthJet)
recoVsTruthHs = h <$= swap . bimap (view bFrag) truthJetBFrag
  where
    h =
      hist2DDef
        (binD 0 22 1.1)
        (binD 0 22 1.1)
        "true $z_{p_{\\mathrm T}}$"
        "reco $z_{p_{\\mathrm T}}$"
        "/recobfragvstruebfrag"

    -- g e = do
    --   tjs <- view truthJets e
    --   let js = view jets e


truthMatchedProbeJets :: Event -> [Corrected SF (Jet, TruthJet)]
truthMatchedProbeJets e =
  let tjs = concat . maybeToList $ view truthjets e
      js = probeJets e
      ms = (fmap.fmap) (`matchJTJ` tjs) js
  in catMaybes $ fmap sequence ms

eventHs :: Fill Event
eventHs = mconcat
  [ muH
  , prefixYF "/met" . over (traverse.xlabel) ("$E_{\\rm T}^{\\rm miss}$ " <>)
    <$> ptH <$= view met
  , prefixYF "/jets" . over (traverse.xlabel) ("jet " <>)
    <$> F.handles (to sequence.folded) lvHs <$= view jets
  , prefixYF "/electrons" . over (traverse.xlabel) ("electron " <>)
    <$> F.handles (to sequence.folded) electronHs <$= view electrons
  , prefixYF "/muons" . over (traverse.xlabel) ("muon " <>)
    <$> F.handles (to sequence.folded) muonHs <$= view muons
  , prefixYF "/probejets" . over (traverse.xlabel) ("probe jet " <>)
    <$> jetHs <$$$= probeJets
  , fmap (prefixYF "/truthrecojets")
    $ recoVsTruthHs <$$$= truthMatchedProbeJets
  ]



runNumber :: Lens' Event Int
runNumber = lens _runNumber $ \e x -> e { _runNumber = x }

eventNumber :: Lens' Event Int
eventNumber = lens _eventNumber $ \e x -> e { _eventNumber = x }

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


type SystMap = M.Map T.Text


withWeights :: Fill a -> F.Fold (CorrectedT SF SystMap a) (SystMap YodaFolder)
withWeights (F.Fold comb start done) = F.Fold comb' start' done'
  where
    start' = mempty

    -- h & at k %~ f xw won't work here
    -- because it uses the lazy variant of maps.
    comb' mh =
      M.foldrWithKey (\k xw h -> M.alter (f xw) k h) mh . runCorrectedT

    f xw Nothing  = Just $ comb start (withCorrection xw)
    f xw (Just h) = Just $ comb h (withCorrection xw)

    done' = fmap done


probeJets :: Event -> [Corrected SF Jet]
probeJets evt =
  case view jets evt of
    [j1, j2] -> probeJet j1 j2 ++ probeJet j2 j1
    _        -> []
  where
    probeJet j j' = sequenceA $ do
      bt <- bTagged j
      if bt && hasSV j'
        then return [j']
        else return []


elmujj :: Event -> Bool
elmujj e =
  let els = _electrons e
      mus = _muons e
      js = _jets e
  in length els == 1
      && all ((> 28) . view lvPt) els
      && length mus == 1
      && all ((> 28) . view lvPt) mus
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
