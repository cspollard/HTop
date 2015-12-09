module Data.HEP.Cut where

import Data.Foldable (Foldable(..))
import qualified Data.Foldable as F

import Control.Arrow

-- Make use of Any, All, etc
type Cut a = a -> Bool

cPass :: Cut a
cPass = const True

cFail :: Cut a
cFail = const False

cOr :: Cut a -> Cut a -> Cut a
cOr c c' = c &&& c' >>> uncurry (||)

cAnd :: Cut a -> Cut a -> Cut a
cAnd c c' = c &&& c' >>> uncurry (&&)

-- TODO
-- make work for any Foldable
cAny :: Foldable t => t (Cut a) -> Cut a
cAny = F.foldr cOr cFail

cAll :: Foldable t => t (Cut a) -> Cut a
cAll = F.foldr cAnd cPass
