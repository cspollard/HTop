{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module BFrag.RecoEvent
  ( module X
  , RecoEvent(RecoEvent)
  , mu, electrons, muons, jets, met
  , readRecoEvent, recoEventHs, probeJets
  , elmujj
  ) where


import           Atlas
import           BFrag.BFrag       as X
import           BFrag.Electron    as X
import           BFrag.Jet         as X
import           BFrag.Muon        as X
import           BFrag.PtEtaPhiE   as X
import           BFrag.Systematics as X
import           BFrag.TrueJet
import           Control.Lens
import           Control.Monad     (guard, join)
import Data.Annotated
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
  => DataMC' -> [BHadron] -> TreeRead m (PhysObj RecoEvent)
readRecoEvent dmc bhs = do
  w <- recoWgt dmc
  mu' <- muVars dmc . float2Double <$> readBranch "mu"
  els <- readElectrons dmc
  mus <- readMuons dmc
  js <- readJets dmc bhs
  met' <- readMET
  return $ w >> return (RecoEvent mu' els mus js met')


muH :: Fills RecoEvent
muH = h =$<< poFromVars . view mu
  where
    h = histo1DDef (evenBins' 0 25 100) "\\ensuremath{< \\mu >}" (dsigdXpbY "<\\mu>" "1") "/mu"


elmujj :: RecoEvent -> PhysObj RecoEvent
elmujj e = do
  let [_el] = _electrons e
      [_mu] = _muons e
      [j1, j2] = _jets e
  guard $ lvDREta j1 j2 > 1.0
  return e


fakeEvent :: RecoEvent -> PhysObj RecoEvent
fakeEvent e = do
  let [el'] = _electrons e
      [mu'] = _muons e
  guard $ not (ePrompt el' && mPrompt mu')
  return e


recoEventHs :: Fills RecoEvent
recoEventHs =
  hs
  <> channel "/fakes" (hs =$<< fakeEvent)
  <> channel "/mu_gt_40" (hs =$<< muCut (> 40))
  <> channel "/mu_lt_40" (hs =$<< muCut (< 40))

  where
    muCut :: (Double -> Bool) -> RecoEvent -> PhysObj RecoEvent
    muCut f e = do
      m <- poFromVars $ view mu e
      guard $ f m
      return e

    hs =
      channel "/elmujj"
      $ mconcat
        [ muH
        , channel "/met"
          $ set (traverse.xlabel) (Just "\\ensuremath{E_{\\rm T}^{\\rm miss}} [GeV]")
            <$> ptH <$= fmap (view met)

        , channel "/jets"
          $ over (traverse.xlabel._Just) ("jet " <>)
            <$> foldlMoore lvHs <$= collapsePO . fmap (view jets)

        , channel "/electrons"
          $ over (traverse.xlabel._Just) ("electron " <>)
            <$> foldlMoore lvHs <$= collapsePO . fmap (view electrons)

        , channel "/muons"
          $ over (traverse.xlabel._Just) ("muon " <>)
            <$> foldlMoore lvHs <$= collapsePO . fmap (view muons)

        , channel "/probejets"
          $ over (traverse.xlabel._Just) ("probe jet " <>)
            <$> foldlMoore (bfragHs `mappend` lvHs `mappend` hadronLabelH)
            <$= fmap join . collapsePO . fmap probeJets
        ] =$<< elmujj



-- note:
-- AnalysisTop selection requires == 2 jets with pT > 30 GeV
probeJets :: RecoEvent -> [PhysObj Jet]
probeJets revt = fmap join . collapsePO . return $
  case view jets revt of
    [j1, j2] ->
      if (lvDREta j1 j2 > 1.0)
        then [probeJet j1 j2, probeJet j2 j1]
        else []
    _ -> []

  where
    probeJet :: Jet -> Jet -> PhysObj Jet
    probeJet j j' = do
      bt <- view isBTagged j
      sv <- hasSV j'
      trks <- svChargedConstits j'
      let pt' = view lvPt j'
      guard
        $ bt
          && sv
          && (view lvAbsEta j' < 2.1)
          && length trks >= 3
          && pt' >= 30
      return j'
