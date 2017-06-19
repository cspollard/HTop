{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module BFrag.TrueEvent
  ( module X
  , TrueEvent(TrueEvent), trueJets, trueElectrons, trueMuons
  , trueEventHs, readTrueEvent, elmujjTrue
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


data TrueEvent =
  TrueEvent
    { _trueJets      :: [TrueJet]
    , _trueElectrons :: [TrueElectron]
    , _trueMuons     :: [TrueMuon]
    } deriving (Generic, Show)


readTrueEvent :: (MonadIO m, MonadThrow m) => TreeRead m (PhysObj TrueEvent)
readTrueEvent = do
  js <- readTrueJets
  es <- readTrueElectrons
  ms <- readTrueMuons
  w <- trueWgt
  return $ w >> pure (TrueEvent js es ms)


trueJets :: Lens' TrueEvent [TrueJet]
trueJets = lens _trueJets $ \te x -> te { _trueJets = x }

trueMuons :: Lens' TrueEvent [TrueMuon]
trueMuons = lens _trueMuons $ \te x -> te { _trueMuons = x }

trueElectrons :: Lens' TrueEvent [TrueElectron]
trueElectrons = lens _trueElectrons $ \te x -> te { _trueElectrons = x }


trueEventHs :: Fills TrueEvent
trueEventHs =
  channelWithLabel "/elmujjtrue" (return . elmujjTrue)
  $ prefixF "/truejets"
    . over (traverse.traverse.xlabel) ("true jet " <>)
    <$> F.handles folded bfragHs <$= sequenceL . fmap (view trueJets)


elmujjTrue :: TrueEvent -> Bool
elmujjTrue te =
  let ne = lengthOf trueElectrons te
      nm = lengthOf trueMuons te
      js = view trueJets te
      bjs = filter trueBJet js
  in ne == 1 && nm == 1 && length js == 2 && length bjs == 2
