{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Atlas.Event ( Event(..)
                        ) where

import Control.Lens

import Data.Serialize
import GHC.Generics (Generic)

import Data.TTree

import Data.HEP.LorentzVector
import Data.Atlas.PtEtaPhiE
import Data.Atlas.Electron
import Data.Atlas.Muon
import Data.Atlas.Jet


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

-- makeLenses ''Event

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
