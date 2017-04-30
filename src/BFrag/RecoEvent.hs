{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module BFrag.RecoEvent
  ( module X
  , RecoEvent(RecoEvent)
  , mu, electrons, muons, jets, met
  , readRecoEvent, recoEventHs
  , probeJets, elmujj
  ) where

import           Atlas
import           BFrag.BFrag       as X
import           BFrag.Electron    as X
import           BFrag.Jet         as X
import           BFrag.Muon        as X
import           BFrag.PtEtaPhiE   as X
import           BFrag.Systematics as X
import qualified Control.Foldl     as F
import           Control.Lens
import           Data.Semigroup
import           Data.TTree
import           GHC.Float
import           GHC.Generics      (Generic)

data RecoEvent =
  RecoEvent
    { _mu        :: Vars Double
    -- , _nPV       :: Double
    , _electrons :: [Electron]
    , _muons     :: [Muon]
    , _jets      :: [Jet]
    , _met       :: PtEtaPhiE
    } deriving (Generic, Show)


mu :: Lens' RecoEvent (Vars Double)
mu = lens _mu $ \e x -> e { _mu = x }

-- nPV :: Lens' RecoEvent Double
-- nPV = lens _nPV $ \e x -> e { _nPV = x }

electrons :: Lens' RecoEvent [Electron]
electrons = lens _electrons $ \e x -> e { _electrons = x }

muons :: Lens' RecoEvent [Muon]
muons = lens _muons $ \e x -> e { _muons = x }

jets :: Lens' RecoEvent [Jet]
jets = lens _jets $ \e x -> e { _jets = x }

met :: Lens' RecoEvent PtEtaPhiE
met = lens _met $ \e x -> e { _met = x }

readMET :: (MonadIO m, MonadFail m) => String -> String -> TreeRead m PtEtaPhiE
readMET m p = do
  et <- float2Double <$> readBranch m
  phi <- float2Double <$> readBranch p
  return $ PtEtaPhiE et 0 phi et

muVars :: DataMC' -> Double -> Vars Double
muVars Data' m =
    -- TODO
    -- here we assume the scaling has already taken place...
    pure m
      & variations . ix "datapileupup" .~ m*1.09
      & variations . ix "datapileupdown" .~ m/1.09
muVars _ m = pure m

readRecoEvent
  :: (MonadIO m, MonadFail m)
  => DataMC' -> TreeRead m (PhysObj RecoEvent)
readRecoEvent dmc = do
  wgt <- evtWgt dmc
  evt <-
    RecoEvent
    <$> (muVars dmc . float2Double <$> readBranch "mu")
    -- <*> (float2Double <$> readBranch "NPVtx")
    <*> readElectrons
    <*> readMuons
    <*> readJets dmc
    <*> readMET "ETMiss" "ETMissPhi"

  return $ onlySFVars wgt evt


muH :: Fills RecoEvent
muH =
  fmap (singleton "/mu")
  . innerF (onlyObjVars . view mu)
  $ hist1DDef (binD 0 25 100) "$< \\mu >$" (dndx "<\\mu>" "1")

npvH :: Fills RecoEvent
npvH =
  fmap (singleton "/npv")
  . innerF (onlyObjVars . view mu)
  $ hist1DDef (binD 0 25 50) "$< \\mu >$" (dndx "npv" "1")

metH :: Fills RecoEvent
metH =
  prefixF "/met"
  . over (traverse.traverse.xlabel) ("$E_{\\rm T}^{\\rm miss}$ " <>)
  <$> ptH
  <$= view met


elmujj :: RecoEvent -> PhysObj Bool
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

recoEventHs :: Fills RecoEvent
recoEventHs =
  mconcat
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
      ]


-- TODO
-- can I make this into a traversal instead of a list?
probeJets :: RecoEvent -> [PhysObj Jet]
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

-- probeJetHs :: Fills RecoEvent
-- probeJetHs = mconcat
--   [ prefixF "/probejets" . over (traverse.traverse.xlabel) ("probe jet " <>)
--     <$> F.premap (fmap join . sequenceA) (F.handles folded jetHs)
--     <$= (\e -> fmap (trueMatch . fromMaybe [] $ view truejets e) <$> probeJets e)
--   ]
