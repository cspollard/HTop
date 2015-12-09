module Data.HEP.Cut where

import Data.Functor.Contravariant
import Data.Foldable (Foldable(..))
import qualified Data.Foldable as F

import Control.Applicative

-- Make use of Any, All, etc
newtype Cut a = Cut { cut :: a -> Bool }

instance Contravariant Cut where
    f `contramap` Cut g = Cut (g . f)

cPass :: Cut a
cPass = Cut $ const True

cFail :: Cut a
cFail = Cut $ const False

cOr :: Cut a -> Cut a -> Cut a
cOr cf cg = Cut $ (||) <$> cut cf <*> cut cg

cAnd :: Cut a -> Cut a -> Cut a
cAnd cf cg = Cut $ (&&) <$> cut cf <*> cut cg

-- TODO
-- make work for any Foldable
cAny :: Foldable t => t (Cut a) -> Cut a
cAny = F.foldr cOr cFail

cAll :: Foldable t => t (Cut a) -> Cut a
cAll = F.foldr cAnd cPass
