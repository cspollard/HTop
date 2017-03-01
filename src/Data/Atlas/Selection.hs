module Data.Atlas.Selection where

{- TODO
-- a cut can fail and always comes with an (in)efficiency SF
newtype Cut a = Cut (a -> (Double, Maybe a))

instance Semigroup (Cut a) where
    Cut f <> Cut g = Cut $ \x ->
                        case f x of
                            ds@(_, Nothing) -> ds
                            (d, Just x') ->
                                let (d', x'') = g x'
                                n  (d*d', x'')
-}
