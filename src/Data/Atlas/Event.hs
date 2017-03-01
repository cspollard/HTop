{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}

module Data.Atlas.Event
  ( Event(..)
  , module X
  , runNumber, eventNumber, mu
  , electrons, muons, jets, met
  , eventHs, overlapRemoval
  , SystMap, withWeights
  , readEvent, elmujj, pruneJets
  ) where

import qualified Control.Foldl            as F
import           Control.Lens
import           Data.HEP.LorentzVector   as X
import qualified Data.Map.Strict          as M
import           Data.Monoid
import qualified Data.Text                as T
import           Data.TTree
import           GHC.Float
import           GHC.Generics             (Generic)

import           Data.Atlas.Electron      as X
import           Data.Atlas.Histogramming
import           Data.Atlas.Jet           as X
import           Data.Atlas.Muon          as X
import           Data.Atlas.PtEtaPhiE     as X

data Event =
  Event
    { _runNumber   :: Int
    , _eventNumber :: Int
    , _mu          :: Double
    , _electrons   :: [Electron]
    , _muons       :: [Muon]
    , _jets        :: [Jet]
    , _met         :: PtEtaPhiE
    } deriving (Generic, Show)



readMET :: MonadIO m => String -> String -> TR m PtEtaPhiE
readMET m p = do
  et <- float2Double <$> readBranch m
  phi <- float2Double <$> readBranch p
  return $ PtEtaPhiE et 0 phi et



readEvent :: MonadIO m => Bool -> TR m Event
readEvent isData =
    Event
      <$> fmap ci2i (readBranch "Run")
      <*> fmap ci2i (readBranch "Event")
      <*> fmap float2Double (readBranch "Mu")
      <*> readElectrons
      <*> readMuons
      <*> readJets isData
      <*> readMET "ETMiss" "ETMissPhi"
    where
      ci2i :: CInt -> Int
      ci2i = fromEnum

muH :: Fill Event
muH =
  hist1DDef
    (binD 0 25 100)
    "$< \\mu >$"
    (dsigdXpbY "\\mu" "1")
    "/mu"
    <$$= mu

eventHs :: Fill Event
eventHs =
  muH
    <> (prefixYF "/met" . over (traverse.xlabel) ("$E_{\\rm T}^{\\rm miss}$ " <>)
        <$> ptH <$$= met)
    <> (prefixYF "/jets" . over (traverse.xlabel) ("jet " <>)
        <$> F.handles (to distribute . folded) lvHs <$$= jets)
    <> (prefixYF "/electrons" . over (traverse.xlabel) ("electron " <>)
        <$> F.handles (to distribute . folded) electronHs <$$= electrons)
    <> (prefixYF "/muons" . over (traverse.xlabel) ("muon " <>)
        <$> F.handles (to distribute . folded) muonHs <$$= muons)
    <> (prefixYF "/probejets" . over (traverse.xlabel) ("probe jet " <>)
        <$> F.handles (to distribute . folded) jetHs <$$= jets . to probeJets)
  where distribute (fs, x) = (,) <$> fs <*> pure x


overlapRemoval :: Event -> Event
overlapRemoval evt = over jets (filter filt) evt
  where
    leps = view electrons evt
    filt = not . any (< 0.2) . traverse lvDREta leps


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

type SystMap = M.Map T.Text

withWeights :: Fill a -> F.Fold (a, SystMap Double) (SystMap YodaFolder)
withWeights (F.Fold comb start done) = F.Fold comb' start' done'
  where
    start' = M.empty
    -- comb' :: M.Map T.Text x -> (a, M.Map T.Text Double) -> M.Map T.Text x
    comb' mh (x, mw) = foldl (\h (k, xw) -> M.alter (f xw) k h) mh (M.toList $ (x,) <$> mw)
    f xw Nothing  = Just $ comb start xw
    f xw (Just h) = Just $ comb h xw
    done' = fmap done


probeJets :: [Jet] -> [Jet]
probeJets [j1, j2] = [j2 | probeJet j1 j2] ++ [j1 | probeJet j2 j1]
    where probeJet j j' = bTagged j && hasSV j'
probeJets _        = []

elmujj :: Event -> Bool
elmujj e = length (_electrons e) == 1 && length (_muons e) == 1 && length (_jets e) == 2

pruneJets :: Event -> Event
pruneJets = over jets $ filter (\j -> view lvPt j > 30 && view lvAbsEta j < 2.1)
