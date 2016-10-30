module Data.Atlas.Selection where

import Control.Lens
import Data.Semigroup

import Data.Atlas.Event

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

elmujj :: Event a -> Bool
elmujj e = length (_electrons e) == 1 && length (_muons e) == 1 && length (_jets e) == 2

pruneJets :: Event a -> Event a
pruneJets = over jets $ filter (\j -> view lvPt j > 30 && view lvAbsEta j < 2.1)

bLabeled :: Jet MC -> Bool
bLabeled = (== 5) . view mcInfo

cLabeled :: Jet MC -> Bool
cLabeled = (== 4) . view mcInfo

lLabeled :: Jet MC -> Bool
lLabeled = ((&&) <$> (/= 5) <*> (/= 4)) . view mcInfo

bTagged :: Jet a -> Bool
bTagged = (> 0.8244273) . view mv2c10

hasSV :: Jet a -> Bool
hasSV = not . null . view svTracks

nSVTracks :: Jet a -> Int
nSVTracks = length . view svTracks

probeJets :: [Jet a] -> [Jet a]
probeJets [j1, j2] = [j2 | probeJet j1 j2] ++ [j1 | probeJet j2 j1]
    where probeJet j j' = bTagged j && hasSV j'
probeJets _        = []
