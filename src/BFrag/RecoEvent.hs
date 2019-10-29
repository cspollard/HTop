{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE RecordWildCards        #-}

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


metH :: Fills RecoEvent
metH = premap (fmap $ view met) h
  where
    h =
      set (_1.traverse.xlabel) (Just "\\ensuremath{E_{\\rm T}^{\\rm miss}} [GeV]")
      <$> ptH


elmujj :: RecoEvent -> PhysObj RecoEvent
elmujj e@RecoEvent{..} = do
  guard $ length _electrons == 1
  guard $ length _muons == 1
  guard $ length _jets >= 2
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

    hs :: Fills RecoEvent
    hs =
      channel "/elmujj"
      $ mconcat
        [ muH
        , channel "/met" metH

        , channel "/jets"
          $ prefixXlabel "jet "
            <$> foldlMoore lvHs <$= collapsePO . fmap (view jets)

        , channel "/electrons"
          $ prefixXlabel "electron "
            <$> foldlMoore lvHs <$= collapsePO . fmap (view electrons)

        , channel "/muons"
          $ prefixXlabel "muon "
            <$> foldlMoore lvHs <$= collapsePO . fmap (view muons)

        , channel "/probejets"
          $ prefixXlabel "probe jet "
            <$> foldlMoore (bfragHs `mappend` lvHs `mappend` hadronLabelH)
            <$= fmap join . collapsePO . fmap probeJets
        ] =$<< elmujj


    prefixXlabel s = bimap go go
      where
        go :: Traversable t => t (Annotated a) -> t (Annotated a)
        go = over (traverse.xlabel._Just) (s <>)



-- note:
-- AnalysisTop selection requires == 2 jets with pT > 30 GeV
probeJets :: RecoEvent -> [PhysObj Jet]
probeJets revt =
  fmap join
  . collapsePO
  . return
  . fmap (uncurry probeJet)
  $ combinations [] (view jets revt)

  where
    combinations _ [] = []
    combinations ls (x:rs) = (x, ls ++ rs) : combinations (x:ls) rs

    probeJet :: Jet -> [Jet] -> PhysObj Jet
    probeJet j js = do
      bt <- traverse (view isBTagged) js
      trks <- svChargedConstits j

      let drs = lvDREta j <$> js

      guard $ view lvAbsEta j < 2.1
      guard $ view lvPt j >= 30
      guard $ length trks >= 3

      guard $ all (>= 1.0) drs

      guard $ or bt
      guard =<< hasSV j

      return j
