{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE CPP #-}

module Data.Atlas.Event ( Event(..)
                        , module X
                        ) where

import Control.Lens

import Data.Serialize
import GHC.Generics (Generic)

import Data.TTree

import Data.HEP.LorentzVector as X hiding (PtEtaPhiEs)
import Data.Atlas.PtEtaPhiE as X
import Data.Atlas.Electron as X
import Data.Atlas.Muon as X
import Data.Atlas.Jet as X


metFromTTree :: MonadIO m => String -> String -> TTreeRead m PtEtaPhiE
metFromTTree m p = do et <- readBranch m
                      phi <- readBranch p
                      return $ PtEtaPhiE et 0 phi et

data Event = Event { _runNumber :: Int
                   , _eventNumber :: Int
                   , _mu :: Float
                   , _electrons :: Electrons
                   , _muons :: Muons
                   , _jets :: Jets
                   , _met :: PtEtaPhiE
                   } deriving (Show, Generic)


runNumber :: Lens' Event Int
runNumber = lens _runNumber $ \e x -> e { _runNumber = x }

eventNumber :: Lens' Event Int
eventNumber = lens _eventNumber $ \e x -> e { _eventNumber = x }

mu :: Lens' Event Float
mu = lens _mu $ \e x -> e { _mu = x }

electrons :: Lens' Event Electrons
electrons = lens _electrons $ \e x -> e { _electrons = x }

muons :: Lens' Event Muons
muons = lens _muons $ \e x -> e { _muons = x }

jets :: Lens' Event Jets
jets = lens _jets $ \e x -> e { _jets = x }

met :: Lens' Event PtEtaPhiE
met = lens _met $ \e x -> e { _met = x }

instance Serialize Event where


instance FromTTree Event where
    fromTTree = Event <$> readBranch "Run"
                      <*> readBranch "Event"
                      <*> readBranch "Mu"
                      <*> fromTTree
                      <*> fromTTree
                      <*> fromTTree
                      <*> metFromTTree "ETMiss" "ETMissPhi"

-- TODO
-- How do we want to deal with syst weights?

data MC a = MC a

-- FromTree a => instance (FromTree MC a) where
