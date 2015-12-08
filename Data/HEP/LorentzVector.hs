{-# LANGUAGE DeriveGeneric #-}

module Data.HEP.LorentzVector where

import Data.Binary
import GHC.Generics (Generic)

import Data.Monoid
import Control.Applicative ((<$>), (<*>))

-- right now you must provide X, Y, Z, T or Pt, Eta, Phi, E definitions
-- I realize this is slow in some cases.
-- TODO

class LorentzVector a where
    lvX :: a -> Double
    lvX = (*) <$> lvPt <*> (cos . lvPhi)
    lvY :: a -> Double
    lvY = (*) <$> lvPt <*> (sin . lvPhi)
    lvZ :: a -> Double
    lvZ = (*) <$> lvPt <*> (sinh . lvEta)
    lvT :: a -> Double
    lvT = lvE

    lvPt :: a -> Double
    lvPt = sqrt . lvPt2
    lvEta :: a -> Double
    lvEta = negate . log . tan . (/2) <$> lvTheta
    lvPhi :: a -> Double
    lvPhi = fmap atan $ (/) <$> lvY <*> lvX
    lvE :: a -> Double
    lvE = lvT

    fromLV :: LorentzVector b => b -> a


class HasLorentzVector hlv where
    lv :: LorentzVector a => hlv -> a


-- flip only the 3 vector of a LorentzVector
lvNegate :: LorentzVector v => v -> v
lvNegate = fmap fromLV $ XYZT <$>
                    (negate . lvX) <*>
                    (negate . lvY) <*>
                    (negate . lvZ) <*>
                    lvT

lvDot :: (LorentzVector v) => v -> v -> Double
lvDot a b = lvX a * lvX b +
            lvY a * lvY b +
            lvZ a * lvZ b -
            lvT a * lvT b

-- TODO
-- some of these should be moved inside LorentzVector for speedup.
lvPx :: (LorentzVector a) => a -> Double
lvPx = lvX

lvPy :: (LorentzVector a) => a -> Double
lvPy = lvY

lvPz :: (LorentzVector a) => a -> Double
lvPz = lvZ

lvE2 :: (LorentzVector a) => a -> Double
lvE2 = lvT2

lvTheta :: (LorentzVector a) => a -> Double
lvTheta = fmap atan $ (/) <$> lvPt <*> lvPz

squareF :: Num b => (a -> b) -> a -> b
squareF f = (*) <$> f <*> f

lvX2 :: (LorentzVector a) => a -> Double
lvX2 = squareF lvX

lvY2 :: (LorentzVector a) => a -> Double
lvY2 = squareF lvY

lvZ2 :: (LorentzVector a) => a -> Double
lvZ2 = squareF lvZ

lvT2 :: (LorentzVector a) => a -> Double
lvT2 = squareF lvT


lvP2 :: (LorentzVector a) => a -> Double
lvP2 = (+) <$> lvPt2 <*> lvZ2

lvPt2 :: (LorentzVector a) => a -> Double
lvPt2 = (+) <$> lvX2 <*> lvY2

lvM :: (LorentzVector a) => a -> Double
lvM = sqrt . lvM2

lvM2 :: (LorentzVector a) => a -> Double
lvM2 = (-) <$> lvE2 <*> lvP2



data PtEtaPhiE = PtEtaPhiE Double Double Double Double
    deriving (Show, Generic)

instance Binary PtEtaPhiE

instance LorentzVector PtEtaPhiE where
    lvPt (PtEtaPhiE pt _ _ _) = pt
    lvEta (PtEtaPhiE _ eta _ _) = eta
    lvPhi (PtEtaPhiE _ _ phi _) = phi
    lvE (PtEtaPhiE _ _ _ e) = e

    fromLV = PtEtaPhiE <$> lvPt <*> lvEta <*> lvPhi <*> lvE

instance HasLorentzVector PtEtaPhiE where
    lv = fromLV

type PtEtaPhiEs = [PtEtaPhiE]



data XYZT = XYZT Double Double Double Double
    deriving (Show, Generic)

instance Binary XYZT

type XYZTs = [XYZT]


instance LorentzVector XYZT where
    lvX (XYZT x _ _ _) = x
    lvY (XYZT _ y _ _) = y
    lvZ (XYZT _ _ z _) = z
    lvT (XYZT _ _ _ t) = t

    fromLV = XYZT <$> lvX <*> lvY <*> lvZ <*> lvT


instance HasLorentzVector XYZT where
    lv = fromLV

-- all LorentzVectors are monoids under addition
instance Monoid PtEtaPhiE where
    mempty = fromLV $ XYZT 0 0 0 0
    a `mappend` b = fromLV $ XYZT
                (lvX a + lvX b)
                (lvY a + lvY b)
                (lvZ a + lvZ b)
                (lvT a + lvT b)

instance Monoid XYZT where
    mempty = fromLV $ XYZT 0 0 0 0
    a `mappend` b = fromLV $ XYZT
                (lvX a + lvX b)
                (lvY a + lvY b)
                (lvZ a + lvZ b)
                (lvT a + lvT b)

toXYZT :: HasLorentzVector a => a -> XYZT
toXYZT = lv

toPtEtaPhiE :: HasLorentzVector a => a -> PtEtaPhiE
toPtEtaPhiE = lv
