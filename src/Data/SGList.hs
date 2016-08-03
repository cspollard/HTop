{-# LANGUAGE DeriveGeneric
           , TypeFamilies
           , GeneralizedNewtypeDeriving
           , DeriveTraversable #-}

module Data.SGList where

import GHC.Generics

import Data.Semigroup
import Data.Serialize

-- TODO
-- this should wrap all foldables if possible.
--
-- a list wrapper that is a semigroup *based on its inner type*, not
-- (++).
-- some things don't work that well, but it's "easy" for now.
newtype SGList h = SGList { fromSGList :: [h] }
                    deriving (Show, Generic, Foldable, Traversable, Functor)

instance Serialize h => Serialize (SGList h) where

liftSG :: ([a] -> [b]) -> SGList a -> SGList b
liftSG f (SGList xs) = SGList (f xs)

instance Semigroup h => Semigroup (SGList h) where
    SGList xs <> SGList xs' = SGList $ zipWith (<>) xs xs'

instance ScaleW h => ScaleW (SGList h) where
    type W (SGList h) = W h
    scaleW hs w = fmap (flip scaleW w) hs
