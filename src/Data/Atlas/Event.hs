{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Data.Atlas.Event ( Event(..), Events
                        , EventWeights, weight, nTags
                        , module X
                        ) where

import Control.Lens

import Data.HEP.LorentzVector as X
import Data.Atlas.Electron as X
import Data.Atlas.Muon as X
import Data.Atlas.Jet as X

import Data.Map (Map)
import qualified Data.Map as M
import Data.Vector (toList)
import Data.Vector.Serialize ()
import Data.Text (Text)

import Data.Serialize
import GHC.Generics (Generic)

import Data.Serialize.Text ()

data Event = Event { eRunNumber :: Int
                   , eEventNumber :: Int
                   , eMu :: Double
                   , eElectrons :: Electrons
                   , eMuons :: Muons
                   , eJets :: Jets
                   , eLargeJets :: LargeJets
                   , eTrackJets :: TrackJets
                   , eMET :: PtEtaPhiE
                   , elelJ :: Bool
                   , mumuJ :: Bool
                   , elmuJ :: Bool
                   , elnuJ :: Bool
                   , munuJ :: Bool
                   , nunuJ :: Bool
                   } deriving (Show, Generic)

instance Serialize Event where

type Events = [Event]

-- TODO
-- How do we want to deal with syst weights?
type EventWeights = Map Text Double

weight :: EventWeights -> [Text] -> Double
weight ew ts = product $ map (ew M.!) ts

nTags :: Event -> (Int, Int)
nTags e = (nljt, nt - nljt)
    where tjs = eTrackJets e
          nljt = maybe 0 (nGhostTags tjs) (eLargeJets e ^? ix 0)
          nt   = length . filter bTagged . toList $ tjs