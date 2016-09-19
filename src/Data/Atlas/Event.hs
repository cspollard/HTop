{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Atlas.Event ( Event(..)
                        , module X
                        , runNumber, eventNumber, mu
                        , electrons, muons, jets, met
                        ) where

import Control.Lens

import GHC.Generics (Generic)

import GHC.Float

import Data.Monoid (Product(..))

import Data.TTree

import Data.HEP.LorentzVector as X
import Data.Atlas.PtEtaPhiE as X
import Data.Atlas.Electron as X
import Data.Atlas.Muon as X
import Data.Atlas.Jet as X
import Data.Atlas.DataMC as X

data Event a =
    Event
        { _runNumber :: CInt
        , _eventNumber :: CInt
        , _mu :: Float
        , _electrons :: [Electron]
        , _muons :: [Muon]
        , _jets :: [Jet a]
        , _met :: PtEtaPhiE
        , _extraInfo :: ExtraInfo (Event a)
        } deriving Generic

runNumber :: Lens' (Event a) CInt
runNumber = lens _runNumber $ \e x -> e { _runNumber = x }

eventNumber :: Lens' (Event a) CInt
eventNumber = lens _eventNumber $ \e x -> e { _eventNumber = x }

mu :: Lens' (Event a) Float
mu = lens _mu $ \e x -> e { _mu = x }

electrons :: Lens' (Event a) [Electron]
electrons = lens _electrons $ \e x -> e { _electrons = x }

muons :: Lens' (Event a) [Muon]
muons = lens _muons $ \e x -> e { _muons = x }

jets :: Lens' (Event a) [Jet a]
jets = lens _jets $ \e x -> e { _jets = x }

met :: Lens' (Event a) PtEtaPhiE
met = lens _met $ \e x -> e { _met = x }


metFromTTree :: MonadIO m => String -> String -> TR m PtEtaPhiE
metFromTTree m p = do et <- float2Double <$> readBranch m
                      phi <- float2Double <$> readBranch p
                      return $ PtEtaPhiE et 0 phi et


instance HasExtraInfo (Event (MC' a)) where
    type ExtraInfo (Event (MC' a)) = Double
    extraInfo = lens _extraInfo $ \e x -> e { _extraInfo = x }

instance HasExtraInfo (Event Data') where
    type ExtraInfo (Event Data') = ()
    extraInfo = lens _extraInfo $ \e x -> e { _extraInfo = x }


instance FromTTree (Event (MC' a)) where
    fromTTree = Event <$> readBranch "Run"
                      <*> readBranch "Event"
                      <*> readBranch "Mu"
                      <*> fmap fromElectrons fromTTree
                      <*> fmap fromMuons fromTTree
                      <*> fmap fromJets fromTTree
                      <*> metFromTTree "ETMiss" "ETMissPhi"
                      <*> (float2Double . getProduct . foldMap Product <$> mapM readBranch ws)
        where ws = ["SFTot"]


instance FromTTree (Event Data') where
    fromTTree = Event <$> readBranch "Run"
                      <*> readBranch "Event"
                      <*> readBranch "Mu"
                      <*> fmap fromElectrons fromTTree
                      <*> fmap fromMuons fromTTree
                      <*> fmap fromJets fromTTree
                      <*> metFromTTree "ETMiss" "ETMissPhi"
                      <*> return ()
