module Data.Atlas.Selection where

import Data.Atlas.Event

elmujj :: Event -> Bool
elmujj e = length (_electrons e) == 1 && length (_muons e) == 1 && length (_jets e) == 2
