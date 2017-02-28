{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}

module Data.Atlas.Event
  ( Event(..)
  , module X
  , runNumber, eventNumber, mu
  , electrons, muons, jets, met
  ) where

import           Control.Lens

import           GHC.Float
import           GHC.Generics           (Generic)

import           Data.Atlas.Electron    as X
import           Data.Atlas.Jet         as X
import           Data.Atlas.Muon        as X
import           Data.Atlas.PtEtaPhiE   as X
import           Data.Atlas.Systematic  as X
import           Data.HEP.LorentzVector as X
import           Data.TTree

data Event =
  Event
    { _runNumber   :: Int
    , _eventNumber :: Int
    , _mu          :: Double
    , _electrons   :: [Electron]
    , _muons       :: [Muon]
    , _jets        :: [Jet]
    , _met         :: PtEtaPhiE
    } deriving Generic

runNumber :: Lens' Event Int
runNumber = lens _runNumber $ \e x -> e { _runNumber = x }

eventNumber :: Lens' Event Int
eventNumber = lens _eventNumber $ \e x -> e { _eventNumber = x }

mu :: Lens' Event Double
mu = lens _mu $ \e x -> e { _mu = x }

electrons :: Lens' Event [Electron]
electrons = lens _electrons $ \e x -> e { _electrons = x }

muons :: Lens' Event [Muon]
muons = lens _muons $ \e x -> e { _muons = x }

jets :: Lens' Event [Jet]
jets = lens _jets $ \e x -> e { _jets = x }

met :: Lens' Event PtEtaPhiE
met = lens _met $ \e x -> e { _met = x }


metFromTTree :: MonadIO m => String -> String -> TR m PtEtaPhiE
metFromTTree m p = do et <- (/1e3) . float2Double <$> readBranch m
                      phi <- float2Double <$> readBranch p
                      return $ PtEtaPhiE et 0 phi et


instance FromTTree Event where
  fromTTree = do
    isData <- (== (0 :: CInt)) <$> readBranch "sampleID"
    Event
      <$> fmap ci2i (readBranch "Run")
      <*> fmap ci2i (readBranch "Event")
      <*> fmap float2Double (readBranch "Mu")
      <*> readElectrons
      <*> readMuons
      <*> readJets isData
      <*> metFromTTree "ETMiss" "ETMissPhi"
    where
      ci2i :: CInt -> Int
      ci2i = fromEnum
