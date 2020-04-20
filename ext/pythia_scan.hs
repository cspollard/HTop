{-# LANGUAGE RecordWildCards, DeriveFunctor, DeriveFoldable
    , DeriveTraversable, GADTs #-}

import Numeric.AD
import Control.Lens (imap)
import Data.Functor.Compose
import Data.Bifunctor.Join



dataset :: Fractional a => [(Coords a, a)]
dataset =
  (\(x, y, z) -> (Join (x, y), z/2))
  <$> 
    [ (0.855, 0.120, -40.5388704231)
    , (0.855, 0.124, -42.6379599379)
    , (0.855, 0.127, -42.66047018)
    , (0.855, 0.130, -43.0077741386)
    , (0.855, 0.136, -45.4867938302)
    , (0.92, 0.120, -42.77839304)
    , (0.92, 0.124, -38.2313930765)
    , (0.92, 0.127, -42.125707701)
    , (0.92, 0.130, -44.8618333126)
    , (0.92, 0.136, -45.7668886324)
    , (0.97, 0.120, -45.2377274837)
    , (0.97, 0.124, -42.2552437712)
    , (0.97, 0.127, -44.1654105708)
    , (0.97, 0.130, -42.9900739687)
    , (0.97, 0.136, -45.3328668379)
    , (1.05, 0.120, -42.8776405779)
    , (1.05, 0.124, -42.6826589269)
    , (1.05, 0.127, -43.4962849299)
    , (1.05, 0.130, -43.2691597142)
    , (1.05, 0.136, -40.8911944506)
    ]


data Params a =
  Params
  { c00 :: a
  , c01 :: a
  , c02 :: a
  , c10 :: a
  , c20 :: a
  , c11 :: a
  } deriving (Functor, Foldable, Traversable, Show)


type Coords = Join (,)

startparams :: Num a => Params a
startparams = Params 0 0 0 0 0 0

startcoords :: Num a => Coords a
startcoords = Join (0, 0)


eval :: Num a => Coords a -> Params a -> a
eval (Join (x, y)) Params{..} =
  c00 + c01*y + c02*y*y + c10*x + c11*x*y + c20*x*x


chi2 :: Num a => (Coords a, a) -> Params a -> a
chi2 (xy, z) ps = (z - eval xy ps) ^ 2


logLH :: Num a => [(Coords a, a)] -> Params a -> a
logLH pts consts = sum $ flip chi2 consts <$> pts


det :: Num a => Coords (Coords a) -> a
det (Join (Join (a, b), Join (c, d))) = a*d - b*c


invert :: Fractional a => Coords (Coords a) -> Coords (Coords a)
invert m@(Join (Join (a, b), Join (c, d))) =
  let det' = det m
  in fmap (/ det')
      <$> Join (Join (d, -b), Join (-c, a))


bestparams :: (Ord a, Fractional a) => Params a
bestparams =
  last
  . take 1000
  . (startparams :)
  $ conjugateGradientDescent (logLH dataset) startparams


bestfit :: (Double, Params Double, Coords Double, Coords (Coords Double))
bestfit =
  let bps :: (Mode a, Scalar a ~ Double) => Params a
      bps = auto <$> bestparams

      bestloglh = eval bcs bps

      bcs =
        last
        . take 1000
        . (startcoords :)
        $ conjugateGradientDescent (flip eval bps) startcoords

  in (bestloglh, bps, bcs, hessian (flip eval bps) bcs)

