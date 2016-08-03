module Data.HEP.Cut where

import qualified Data.Foldable as F
import Control.Applicative

-- TODO
-- bijections are the "right" way to do cOr and cAnd.
-- Make use of Any, All, etc

type Cut a = a -> Bool

cPass :: Cut a
cPass = const True

cFail :: Cut a
cFail = const False

cOr :: Cut a -> Cut a -> Cut a
cOr = liftA2 (||)

cAnd :: Cut a -> Cut a -> Cut a
cAnd = liftA2 (&&)

cAny :: Foldable t => t (Cut a) -> Cut a
cAny = F.foldr cOr cFail

cAll :: Foldable t => t (Cut a) -> Cut a
cAll = F.foldr cAnd cPass
