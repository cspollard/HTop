{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE RecordWildCards       #-}
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
import qualified Control.Foldl     as F
import           Control.Lens
import           Control.Monad     (guard, join)
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
  => DataMC' -> [BHadron] -> TreeRead m (PhysObj RecoEvent)
readRecoEvent dmc bhs = do
  w <- recoWgt dmc
  mu' <- muVars dmc . float2Double <$> readBranch "mu"
  els <- readElectrons dmc
  mus <- readMuons dmc
  js <- readJets dmc bhs
  met' <- readMET
  return $ w >> return (RecoEvent mu' els mus js met')


muH :: VarFill RecoEvent
muH = h =$<< poFromVars . view mu
  where
    h = hist1DDef (binD 0 25 100) "\\ensuremath{< \\mu >}" (dsigdXpbY "<\\mu>" "1")


elmujj :: RecoEvent -> PhysObj Bool
elmujj RecoEvent{..} =
  return
  $ length _electrons == 1    
    && length _muons == 1    
    && length _jets >= 2    
    


fakeEvent :: RecoEvent -> PhysObj Bool
fakeEvent RecoEvent{..} =
  return $ not (any ePrompt _electrons) || not (any mPrompt _muons)


-- so much boilerplate
recoEventHs :: VarFills RecoEvent
recoEventHs =
  mconcat
  [ hs
  , channelWithLabel "/fakes" fakeEvent hs
  , channelWithLabel "/njets_eq_2" (pure . (==2) . nj) hs
  , channelWithLabel "/njets_gt_2" (pure . (>2) . nj) hs
  , channelWithLabel "/mu_lt_20" (poFromVars . fmap (<20) . view mu) hs
  , channelWithLabel "/mu_gt_20" (poFromVars . fmap (>20) . view mu) hs
  ]

  where
    nj RecoEvent{..} = length _jets

    hs =
          channelWithLabel "/elmujj" elmujj
          $ mconcat
            [ singleton "/mu" <$> muH
            -- , singleton "/npv" <$> npvH

            , singleton "/met/pt"
              . set xlabel "\\ensuremath{E_{\\rm T}^{\\rm miss}} [GeV]"
              <$> ptH
              <$= fmap (view met)

            , prefixF "/jets"
              . over (traverse.xlabel) ("jet " <>)
              <$> F.handles folded lvHs <$= collapsePO . fmap (view jets)

            , prefixF "/electrons"
              . over (traverse.xlabel) ("electron " <>)
              <$> F.handles folded lvHs <$= collapsePO . fmap (view electrons)

            , prefixF "/muons"
              . over (traverse.xlabel) ("muon " <>)
              <$> F.handles folded lvHs <$= collapsePO . fmap (view muons)

            , prefixF "/probejets"
              . over (traverse.xlabel) ("probe jet " <>)
              <$> F.handles folded (bfragHs `mappend` lvHs `mappend` hadronLabelH)
              <$= fmap join . collapsePO . fmap probeJets
            ]



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

      guard $ all (>= 0.5) drs

      guard $ or bt
      guard =<< hasSV j

      return j
