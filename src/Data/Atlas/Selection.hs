module Data.Atlas.Selection where

import Control.Lens

import Data.Atlas.Event

elmujj :: Event a -> Bool
elmujj e = length (_electrons e) == 1 && length (_muons e) == 1 && length (_jets e) == 2

pruneJets :: Event a -> Event a
pruneJets = over jets $ filter (\j -> view lvPt j > 30 && view lvAbsEta j < 2.1)

bLabeled :: Jet (MC' a) -> Bool
bLabeled = (== 5) . view extraInfo

cLabeled :: Jet (MC' a) -> Bool
cLabeled = (== 4) . view extraInfo

lLabeled :: Jet (MC' a) -> Bool
lLabeled = ((&&) <$> (/= 5) <*> (/= 4)) . view extraInfo

bTagged :: Jet a -> Bool
bTagged = (> 0.8244273) . view jMV2c10

hasSV :: Jet a -> Bool
hasSV = not . null . view jSVTracks

nSVTracks :: Jet a -> Int
nSVTracks = length . view jSVTracks

probeJets :: [Jet a] -> [Jet a]
probeJets [j1, j2] = [j2 | probeJet j1 j2] ++ [j1 | probeJet j2 j1]
    where probeJet j j' = bTagged j && hasSV j'
probeJets _        = []
