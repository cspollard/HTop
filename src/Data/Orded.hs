-- TODO
-- this should go somewhere else.

module Data.Orded where

-- wrapper class for ordering
data Orded a b = Orded { orded :: a
                       , fromOrded :: b
                       } deriving Show

ordedBy :: (b -> a) -> b -> Orded a b
ordedBy f x = Orded (f x) x

liftO :: (b -> c) -> Orded a b -> Orded a c
liftO = fmap

instance Eq a => Eq (Orded a b) where
    (Orded x _) == (Orded y _) = x == y

instance Ord a => Ord (Orded a b) where
    (Orded x _) `compare` (Orded y _) = x `compare` y

instance Functor (Orded a) where
    f `fmap` Orded x y = Orded x (f y)

instance Foldable (Orded a) where
    foldr f x (Orded _ y) = f y x

instance Traversable (Orded a) where
    traverse f (Orded x y) = Orded x <$> f y
