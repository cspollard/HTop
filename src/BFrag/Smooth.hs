{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module BFrag.Smooth where

import           Data.Functor.Compose
import           Numeric.AD
import Linear.Matrix


polynomial :: (Num a, Foldable f) => f a -> a -> a
polynomial fc x =
  snd $ foldl (\(n, sofar) c -> (n+1, sofar + c*x^n)) (0::Int, 0) fc


-- try functions of the form Sum_{ij} c_{ij} x**i * y**j
polynomial2D :: (Num a, Foldable f, Foldable g) => Compose f g a -> (a, a) -> a
polynomial2D (Compose fgc) (x, y) = snd $ foldl goi (0::Int, 0) fgc
  where
    goi (i, sofar) gc = (i+1, (+sofar) . (*x^i) . snd $ foldl goj (0::Int, 0) gc)
    goj (j, sofar) c = (j+1, sofar + c*y^j)


fitPoly
  :: (Num a, Num b, Foldable f, Functor g, Foldable g)
  => g (a, a -> b) -> f a -> b
fitPoly hist params = sum $ (\(x, p) -> p $ polynomial params x) <$> hist


fitPoly2D
  :: (Num a, Functor f, Foldable f, Foldable g, Foldable h)
  => f ((a, a), a -> a) -> Compose h g a -> a
fitPoly2D hist params = sum $ (\(x, p) -> p $ polynomial2D params x) <$> hist



normalLogLH :: Fractional a => a -> a -> a -> a
normalLogLH m s x = sqr $ (x-m)/s
  where
    sqr y = y^(2::Int)


polySmooth
  :: (Foldable g, Functor g, Ord a, Fractional a, Traversable f)
  => f a -> Int -> g (a, (a, a)) -> (f a, g (a, a))
polySmooth start steps v = (end, (\(b, _) -> (b, polynomial end b)) <$> v)

  where
    go :: (Mode s, Scalar s ~ a, Fractional s) => (a, (a, a)) -> (s, s -> s)
    go (b, (m, s)) = (auto b, normalLogLH (auto m) (auto s))

    end = last . take steps $ conjugateGradientDescent (fitPoly $ go <$> v) start


polySmooth2D
  :: (Foldable h, Functor h, Ord a, Fractional a, Traversable f, Traversable g)
  => f (g a) -> Int -> h ((a, a), (a, a)) -> (f (g a), h ((a, a), a))
polySmooth2D start steps v = (getCompose end, (\(b, _) -> (b, polynomial2D end b)) <$> v)

  where
    go :: (Mode s, Scalar s ~ a, Fractional s) => ((a, a), (a, a)) -> ((s, s), s -> s)
    go ((i, j), (m, s)) = ((auto i, auto j), normalLogLH (auto m) (auto s))

    end = last . take steps $ conjugateGradientDescent (fitPoly2D $ go <$> v) (Compose start)



-- newton
--   :: (Ord a, Fractional a, Traversable f)
--   => Int -> f a -> (forall b. Num b => f b -> b) -> f a
-- newton steps start func =
--   last . (start :) . take steps
--   $ conjugateGradientDescent func start


-- divergence :: (Num a, Trace m) => (m a -> m a) -> m a -> a
-- divergence f = trace . fmap (grad f)
