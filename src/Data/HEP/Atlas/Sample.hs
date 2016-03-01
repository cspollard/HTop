module Data.HEP.Atlas.Sample where

import Data.HEP.Atlas.Event

data Sample = SingleSample Int Int Double Double Events
            | CombinedSample [Sample]

dsids :: Sample -> [Int]
dsids (SingleSample x _ _ _ _) = [x]
dsids (CombinedSample ss) = concatMap dsids ss

numEvents :: Sample -> Int
numEvents (SingleSample _ x _ _ _) = x
numEvents (CombinedSample ss) = sum . map numEvents $ ss

sumWeights :: Sample -> Double
sumWeights (SingleSample _ _ x _ _) = x
sumWeights (CombinedSample ss) = sum . map sumWeights $ ss

crossSection :: Sample -> Double
crossSection (SingleSample _ _ _ x _) = x
crossSection (CombinedSample ss) = sum . map crossSection $ ss

events :: Sample -> Events
events (SingleSample _ _ _ _ x) = x
events (CombinedSample ss) = concatMap events ss
