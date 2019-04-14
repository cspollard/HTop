{-# LANGUAGE TypeFamilies #-}

module BFrag.Smooth where

import           Numeric.AD


polynomial :: (Num a, Foldable f) => f a -> a -> a
polynomial fc x =
  snd $ foldl (\(n, sofar) c -> (n+1, sofar + c*x^n)) (0::Int, 0) fc


fitPoly
  :: (Num a, Foldable f, Functor g, Foldable g)
  => g (a, a -> a) -> f a -> a
fitPoly hist params = sum $ (\(x, p) -> p $ polynomial params x) <$> hist


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

    end = last . take steps $ gradientDescent (fitPoly $ go <$> v) start
