module Data.Atlas.Selection where

import           Control.Lens

import           Data.Atlas.Event

{- TODO
-- a cut can fail and always comes with an (in)efficiency SF
newtype Cut a = Cut (a -> (Double, Maybe a))

instance Semigroup (Cut a) where
    Cut f <> Cut g = Cut $ \x ->
                        case f x of
                            ds@(_, Nothing) -> ds
                            (d, Just x') ->
                                let (d', x'') = g x'
                                n  (d*d', x'')
-}

elmujj :: Event -> Bool
elmujj e = length (_electrons e) == 1 && length (_muons e) == 1 && length (_jets e) == 2

pruneJets :: Event -> Event
pruneJets = over jets $ filter (\j -> view lvPt j > 30 && view lvAbsEta j < 2.1)

bLabeled :: Jet -> Bool
bLabeled = views truthFlavor (== Just B)

cLabeled :: Jet -> Bool
cLabeled = views truthFlavor (== Just C)

lLabeled :: Jet -> Bool
lLabeled = views truthFlavor (== Just L)

bTagged :: Jet -> Bool
bTagged = views mv2c10 (> 0.8244273)

hasSV :: Jet -> Bool
hasSV = views svTracks (not . null)

probeJets :: [Jet] -> [Jet]
probeJets [j1, j2] = [j2 | probeJet j1 j2] ++ [j1 | probeJet j2 j1]
    where probeJet j j' = bTagged j && hasSV j'
probeJets _        = []
