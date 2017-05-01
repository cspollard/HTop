{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module BFrag.TrueEvent
  ( module X
  , TrueEvent(TrueEvent), trueJets
  , trueEventHs, readTrueEvent
  ) where

import           Atlas
import           BFrag.TrueJet  as X
import qualified Control.Foldl  as F
import           Control.Lens
import           Data.Semigroup
import           Data.TTree
import           GHC.Generics   (Generic)

data TrueEvent =
  TrueEvent
    { _trueJets :: [TrueJet]
    } deriving (Generic, Show)


readTrueEvent :: (MonadIO m, MonadFail m) => TreeRead m (PhysObj TrueEvent)
readTrueEvent = pure . TrueEvent <$> readTrueJets

trueJets :: Lens' TrueEvent [TrueJet]
trueJets = lens _trueJets $ \te x -> te { _trueJets = x }

trueEventHs :: Fills TrueEvent
trueEventHs =
  prefixF "/truthjets" . over (traverse.traverse.xlabel) ("truth jet " <>)
  <$> F.handles (to sequence.folded) trueJetHs
  <$= view trueJets
