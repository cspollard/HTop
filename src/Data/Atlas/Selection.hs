module Data.Atlas.Selection where

import Control.Lens

import Data.Atlas.Event

elmujj :: Event -> Bool
elmujj e = length (_electrons e) == 1 && length (_muons e) == 1 && length (_jets e) == 2

pruneJets :: Event -> Event
pruneJets = over jets $ filter (\j -> let v = lv j :: PtEtaPhiE in lvPt v > 25 && abs (lvEta v) < 2.5)

bTagged :: Jet -> Bool
bTagged = (> 0.8244273) . jMV2c10

hasSV :: Jet -> Bool
hasSV = not . null . jSVTracks

probeJets :: [Jet] -> [Jet]
probeJets [j1, j2] = [j2 | probeJet j1 j2] ++ [j1 | probeJet j2 j1]
    where probeJet j j' = bTagged j && hasSV j'
probeJets _        = []
