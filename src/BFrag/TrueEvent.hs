{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module BFrag.TrueEvent
  ( module X
  , TrueEvent(TrueEvent), trueJets
  , trueEventHs, readTrueEvent
  ) where

import           Atlas
import           BFrag.Systematics
import           BFrag.TrueJet     as X
import           Control.Lens
import           Data.TTree
import           GHC.Generics      (Generic)

data TrueEvent =
  TrueEvent
    { _trueJets :: [TrueJet]
    } deriving (Generic, Show)


readTrueEvent :: (MonadIO m, MonadThrow m) => TreeRead m (PhysObj TrueEvent)
readTrueEvent = do
  js <- readTrueJets
  w <- trueWgt
  return $ w >> pure (TrueEvent js)

trueJets :: Lens' TrueEvent [TrueJet]
trueJets = lens _trueJets $ \te x -> te { _trueJets = x }

trueEventHs :: Fills (TrueEvent, Double)
trueEventHs = mempty
  -- prefixF "/truthjets" . over (traverse.traverse.xlabel) ("truth jet " <>)
  -- <$> F.handles (to sequence.folded) trueJetHs
  -- <$= view trueJets
