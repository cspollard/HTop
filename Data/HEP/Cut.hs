module Data.HEP.Cut where

import Data.Functor.Contravariant

import Control.Applicative

-- Make use of Any, All, etc
newtype Cut a = Cut { cut :: a -> Bool }

instance Contravariant Cut where
    f `contramap` Cut g = Cut (g . f)

cOr :: Cut a -> Cut a -> Cut a
cOr cf cg = Cut $ (||) <$> cut cf <*> cut cg

cAnd :: Cut a -> Cut a -> Cut a
cAnd cf cg = Cut $ (&&) <$> cut cf <*> cut cg

cAny :: [Cut a] -> Cut a
cAny [] = Cut $ const True
cAny cs = foldr1 cOr cs

cAll :: [Cut a] -> Cut a
cAll = foldr cAnd $ Cut (const True)
