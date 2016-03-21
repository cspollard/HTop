-- TODO
-- this should go somewhere else.

module Data.OrdBy where

-- wrapper class for ordering
data OrdBy a b = OrdBy (b -> a) b

instance Eq a => Eq (OrdBy a b) where
    (OrdBy f x) == (OrdBy g y) = f x == g y

instance Ord a => Ord (OrdBy a b) where
    (OrdBy f x) `compare` (OrdBy g y) = f x `compare` g y

fromOrdBy :: OrdBy a b -> b
fromOrdBy (OrdBy _ x) = x
