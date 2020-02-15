{-# LANGUAGE RecordWildCards, DeriveFunctor, DeriveFoldable
    , DeriveTraversable, GADTs #-}

import Numeric.AD
import Control.Lens (imap)
import Data.Functor.Compose
import Data.Bifunctor.Join

dataset :: Fractional a => [(Coords a, a)]
dataset =
  (\(x, y, z) -> (Join (x, y), z))
  <$> 
    [ (0.67, 0.124, -29.7077537966)
    , (0.67, 0.127, -36.9350190718)
    , (0.67, 0.136, -45.1016216078)
    , (0.855, 0.120, -33.0396709225)
    , (0.855, 0.124, -43.7016919021)
    , (0.855, 0.127, -45.2339340979)
    , (0.855, 0.130, -44.9486395585)
    , (0.855, 0.136, -45.9831859531)
    , (0.92, 0.120, -41.8435714138)
    , (0.92, 0.124, -41.3586468438)
    , (0.92, 0.127, -43.3807827007)
    , (0.92, 0.130, -44.946386927)
    , (0.92, 0.136, -46.5519197992)
    , (0.97, 0.120, -44.369378694)
    , (0.97, 0.124, -41.1438710686)
    , (0.97, 0.127, -45.3437092047)
    , (0.97, 0.130, -38.8088849543)
    , (0.97, 0.136, -30.0391750058)
    , (1.05, 0.120, -43.4643100238)
    , (1.05, 0.124, -45.7744457038)
    , (1.05, 0.127, -42.0122504735)
    , (1.05, 0.130, -38.7486986844)
    , (1.05, 0.136, -33.6480541673)
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


invert :: Fractional a => Coords (Coords a) -> Coords (Coords a)
invert (Join (Join (a, b), Join (c, d))) =
  fmap (/ (a*d - b*c))
  <$> Join (Join (d, -b), Join (-c, a))


bestparams :: (Ord a, Fractional a) => Params a
bestparams =
  last
  . take 500
  . (startparams :)
  $ conjugateGradientDescent (logLH dataset) startparams


bestfit :: (Params Double, Coords Double, Coords (Coords Double))
bestfit =
  let bps :: (Mode a, Scalar a ~ Double) => Params a
      bps = auto <$> bestparams
      bcs =
        last
        . take 500
        . (startcoords :)
        $ conjugateGradientDescent (flip eval bps) startcoords
  in (bps, bcs, hessian (flip eval bps) bcs)
