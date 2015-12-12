module Data.Uncertain where

-- this can be made much more general..
-- second argument should actually be a pdf, but...

-- strict in each argument?

-- an uncertain value:
-- - the value
-- - its uncertainty
data U a = U a a
    deriving (Eq, Ord)

instance Show a => Show (U a) where
    show (U x y) = show x ++ " +/- " ++ show y

sqr :: Num a => a -> a
sqr a = a*a

quadsum :: Floating a => a -> a -> a
quadsum x y = sqrt $ sqr x + sqr y

-- TODO
-- abs???
pois :: Floating a => a -> U a
pois x = x +- sqrt (abs x)

constU :: Num a => a -> U a
constU x = U x 0

infixr 9 +-
(+-) :: a -> a -> U a
x +- dx = U x dx

infix 0 ><
(><) :: Num a => (a -> a) -> (a -> a) -> U a -> U a
(f >< f') (U x x') = U (f x) (abs $ x' * f' x)

instance Floating a => Num (U a) where
    fromInteger = constU . fromInteger
    (U x dx) + (U y dy) = U (x+y) (quadsum dx dy)
    (U x dx) * (U y dy) = U (x*y) (quadsum (y*dx) (x*dy))
    negate (U x dx) = U (-x) dx

    -- I'm not sure if these are correct.
    signum (U x _) = U (signum x) 0
    abs (U x dx) = U (abs x) (dx/signum x)


instance Floating a => Fractional (U a) where
    fromRational = constU . fromRational
    recip = recip >< \x -> negate . sqr $ recip x

instance Floating a => Floating (U a) where
    pi = constU pi
    exp = exp >< exp
    log = log >< recip
    sqrt = sqrt >< \x -> recip $ 2*sqrt x
    sin = sin >< cos
    cos = cos >< (negate . sin)
    asin = asin >< \x -> recip (sqrt (1 - sqr x))
    acos = acos >< \x -> -recip (sqrt (1 - sqr x))
    atan = atan >< \x -> recip (sqr x + 1)
    sinh = sinh >< cosh
    cosh = cosh >< sinh
    asinh = asinh >< \x -> recip(sqrt (sqr x + 1))
    acosh = acosh >< \x -> -recip (sqrt (sqr x - 1))
    atanh = atanh >< \x -> recip (1 - sqr x)
