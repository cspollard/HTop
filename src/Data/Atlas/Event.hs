{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Atlas.Event ( Event(..)
                        , module X
                        , runNumber, eventNumber, mu
                        , electrons, muons, jets, met
                        , readEventSysts
                        ) where

import Control.Lens

import GHC.Generics (Generic)
import GHC.Float
import Data.Map.Strict as M
import Data.Text

import Data.TTree
import Data.HEP.LorentzVector as X
import Data.Atlas.PtEtaPhiE as X
import Data.Atlas.Electron as X
import Data.Atlas.Muon as X
import Data.Atlas.Jet as X
import Data.Atlas.DataMC as X
import Data.Atlas.Systematic as X

data Event a =
    Event
        { _runNumber :: CInt
        , _eventNumber :: CInt
        , _mu :: Float
        , _electrons :: [Electron]
        , _muons :: [Muon]
        , _jets :: [Jet a]
        , _met :: PtEtaPhiE
        , _mcInfo :: MCInfo (Event a)
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


instance HasMCInfo (Event MC) where
    type MCInfo (Event MC) = Double
    mcInfo = lens _mcInfo $ \e x -> e { _mcInfo = x }

instance HasMCInfo (Event Data') where
    type MCInfo (Event Data') = ()
    mcInfo = lens _mcInfo $ \e x -> e { _mcInfo = x }


readEventG :: (MonadIO m, FromTTree (Jets a))
           => TR m (MCInfo (Event a) -> Event a)
readEventG =
    Event
        <$> readBranch "Run"
        <*> readBranch "Event"
        <*> readBranch "Mu"
        <*> fmap fromElectrons fromTTree
        <*> fmap fromMuons fromTTree
        <*> fmap fromJets fromTTree
        <*> metFromTTree "ETMiss" "ETMissPhi"


readEventSysts :: MonadIO m
               => [WeightSystematic]
               -> TR m (Map Text (Event MC))
readEventSysts systs = M.fromList <$> evts
    where
        f :: MonadIO m => WeightSystematic -> TR m (Text, Event MC)
        f (WeightSystematic n g) =
            fmap (n,) $ readEventG <*> g

        evts = mapM f systs

instance FromTTree (Event Data') where
    fromTTree = readEventG <*> return ()
