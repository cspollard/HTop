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
    -- [ (0.67, 0.124, -30.7447926555)
    -- , (0.67, 0.127, -35.7247470933)
    -- , (0.67, 0.136, -44.4963836113)
    [ (0.855, 0.120, -40.2590062576)
    , (0.855, 0.124, -43.7176722094)
    , (0.855, 0.127, -45.0768651375)
    , (0.855, 0.130, -44.6250508932)
    , (0.855, 0.136, -45.4049668974)
    , (0.92, 0.120, -45.1636518629)
    , (0.92, 0.124, -44.3303536185)
    , (0.92, 0.127, -45.6138962075)
    , (0.92, 0.130, -47.0150157702)
    , (0.92, 0.136, -42.8543886575)
    , (0.97, 0.120, -46.5026659988)
    , (0.97, 0.124, -47.0305427048)
    , (0.97, 0.127, -43.8569444638)
    , (0.97, 0.130, -45.9927300926)
    -- , (0.97, 0.136, -35.5432750266)
    , (1.05, 0.120, -44.6241277128)
    , (1.05, 0.124, -44.7109240933)
    , (1.05, 0.127, -43.0364098206)
    , (1.05, 0.130, -42.4861967497)
    -- , (1.05, 0.136, -35.0987458459)
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

