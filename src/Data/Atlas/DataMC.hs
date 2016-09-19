{-# LANGUAGE TypeFamilies #-}

module Data.Atlas.DataMC where

import Control.Lens

data Data'
data MC' a

-- no systematic
type Nominal' = MC' ()

class HasExtraInfo a where
    type ExtraInfo a :: * 
    extraInfo :: Lens' a (ExtraInfo a)
