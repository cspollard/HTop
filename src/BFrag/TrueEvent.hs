{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module BFrag.TrueEvent
  ( module X
  , TrueEvent(TrueEvent), trueJets, trueElectrons, trueMuons
  , trueEventHs, readTrueEvent, elmujjTrue, trueProbeJets, trueRho
  ) where

import           Atlas
import           BFrag.BFrag
import           BFrag.Systematics
import           BFrag.TrueElectron as X
import           BFrag.TrueJet      as X
import           BFrag.TrueMuon     as X
import qualified Control.Foldl      as F
import           Control.Lens
import           Data.Semigroup
import           Data.TTree
import           GHC.Generics       (Generic)
import Data.List (partition)


data TrueEvent =
  TrueEvent
  { _trueJets      :: [TrueJet]
  , _trueElectrons :: [TrueElectron]
  , _trueMuons     :: [TrueMuon]
  } deriving (Generic, Show)


readTrueEvent :: (MonadIO m, MonadThrow m) => TreeRead m (TrueEvent, SF)
readTrueEvent = do
  js <- readTrueJets
  es <- readTrueElectrons
  ms <- readTrueMuons
  w <- trueWgt
  return (TrueEvent js es ms, w)


trueJets :: Lens' TrueEvent [TrueJet]
trueJets = lens _trueJets $ \te x -> te { _trueJets = x }

trueMuons :: Lens' TrueEvent [TrueMuon]
trueMuons = lens _trueMuons $ \te x -> te { _trueMuons = x }

trueElectrons :: Lens' TrueEvent [TrueElectron]
trueElectrons = lens _trueElectrons $ \te x -> te { _trueElectrons = x }


trueProbeJets :: TrueEvent -> [(TrueJet, TrueEvent)]
trueProbeJets te =
  let tjs = filter trueProbeJet $ view trueJets te
  in (,te) <$> tjs


trueEventHs :: VarFills TrueEvent
trueEventHs =
  channelWithLabel "/elmujjtrue" (return . elmujjTrue)
  $ prefixF "/truejets" . over (traverse.xlabel) ("true jet " <>)
    <$> twine (fmap return . trueProbeJets)
      ( ((bfragHs `mappend` lvHs `mappend` bHs) <$= fmap fst)
        `mappend` (prebind trueRho $ singleton "/rho" <$> rhoH)
      )

  where
    bHs :: VarFills TrueJet
    bHs =
      F.handles folded (mconcat [bMesonH, bBaryonH])
      <$= collapsePO . fmap (view tjBHadrons)


elmujjTrue :: TrueEvent -> Bool
elmujjTrue te =
  let ne = lengthOf trueElectrons te
      nm = lengthOf trueMuons te
      js = view trueJets te
      (bjs, notbjs) = partition trueBJet js
      wellsep = all (\b -> all ((> 0.5) . lvDREta b) notbjs) bjs
  in case bjs of
    [b1, b2] -> ne == 1 && nm == 1 && (lvDREta b1 b2 > 0.5) && wellsep
    _ -> False


trueRho :: (TrueJet, TrueEvent) -> PhysObj Double
trueRho (j, e) = do
  denom <- lepAvgPt e
  svpt <- svPt j
  return $ svpt / denom

  where
    lepAvgPt :: TrueEvent -> PhysObj Double
    lepAvgPt TrueEvent{..} = do
      let [e] = _trueElectrons
          [m] = _trueMuons
      return $ (view lvPt e + view lvPt m) / 2

    svPt :: TrueJet -> PhysObj Double
    svPt j = view lvPt <$> svChargedTLV j


