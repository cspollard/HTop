{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module BFrag.RecoEvent
  ( module X
  , RecoEvent(RecoEvent)
  , mu, electrons, muons, jets, met
  , readRecoEvent, recoEventHs
  , elmujj
  ) where

import           Atlas
import           BFrag.BFrag       as X
import           BFrag.Electron    as X
import           BFrag.Jet         as X
import           BFrag.Muon        as X
import           BFrag.PtEtaPhiE   as X
import           BFrag.Systematics as X
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

readMET :: (MonadIO m, MonadThrow m) => TreeRead m PtEtaPhiE
readMET = do
  et <- (/1e3) . float2Double <$> readBranch "met_met"
  phi <- (/1e3) . float2Double <$> readBranch "met_phi"
  return $ PtEtaPhiE et 0 phi et


muVars :: DataMC' -> Double -> Vars Double
muVars Data' m =
    -- TODO
    -- here we assume the scaling has already taken place...
    Variation m [("datapileupup", m*1.09), ("datapileupdown", m/1.09)]
muVars _ m = pure m


readRecoEvent
  :: (MonadIO m, MonadThrow m)
  => DataMC' -> TreeRead m (PhysObj RecoEvent)
readRecoEvent dmc = do
  w <- recoWgt dmc
  mu' <- muVars dmc . float2Double <$> readBranch "mu"
  els <- readElectrons
  mus <- readMuons
  js <- readJets dmc
  met' <- readMET
  return $ w >> return (RecoEvent mu' els mus js met')


muH :: Fills RecoEvent
muH =
  singleton "/mu"
  <$> physObjH h =$<< (varObj . view mu)

  where
    h = hist1DDef (binD 0 25 100) "$< \\mu >$" (dndx "<\\mu>" "1")


-- TODO
-- npvH :: FoldM Vars (RecoEvent, Double) (YodaFolder)


metH :: Fills RecoEvent
metH =
  over (traverse.traverse.xlabel) ("$E_{\\rm T}^{\\rm miss}$ " <>)
  . singleton "/met/pt"
  <$> ptH <$= fmap (view met)



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
recoEventHs = mconcat [ metH, muH ]
  -- mconcat
  --   [ muH
  --   , npvH
  --   , metH
  --   , prefixF "/jets" . over (traverse.traverse.xlabel) ("jet " <>)
  --     <$> F.handles (to sequence.folded) lvHs
  --     <$= view jets
  --   , prefixF "/electrons" . over (traverse.traverse.xlabel) ("electron " <>)
  --     <$> F.handles (to sequence.folded) electronHs
  --     <$= view electrons
  --   , prefixF "/muons" . over (traverse.traverse.xlabel) ("muon " <>)
  --     <$> F.handles (to sequence.folded) muonHs
  --     <$= view muons
  --     ]


-- TODO
-- can I make this into a traversal instead of a list?
-- probeJets :: RecoEvent -> [PhysObj Jet]
-- probeJets evt =
--   case view jets evt of
--     [j1, j2] ->
--       if lvDREta j1 j2 > 1.0
--         then probeJet j1 j2 ++ probeJet j2 j1
--         else []
--     _        -> []
--   where
--     probeJet j j' = sequenceA $ do
--       bt <- view isBTagged j
--       if bt && hasSV j' && (view lvAbsEta j' < 2.1)
--         then return [j']
--         else return []
--
-- probeJetHs :: Fills m RecoEvent
-- probeJetHs = mconcat
--   [ prefixF "/probejets" . over (traverse.traverse.xlabel) ("probe jet " <>)
--     <$> F.premap (fmap join . sequenceA) (F.handles folded jetHs)
--     <$= (\e -> fmap (trueMatch . fromMaybe [] $ view truejets e) <$> probeJets e)
--   ]
