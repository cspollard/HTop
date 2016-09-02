module Data.Atlas.Selection where

import Control.Lens

import Data.Atlas.Event

elmujj :: Event -> Bool
elmujj e = length (_electrons e) == 1 && length (_muons e) == 1 && length (_jets e) >= 2

pruneJets :: Event -> Event
pruneJets = over jets $ filter (\j -> let v = lv j :: PtEtaPhiE in lvPt v > 25 && abs (lvEta v) < 2.5)
