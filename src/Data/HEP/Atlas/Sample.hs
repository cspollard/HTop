{-# LANGUAGE DeriveGeneric #-}

module Data.HEP.Atlas.Sample where

import GHC.Generics
import Data.Binary

import Data.HEP.Atlas.Event
import Data.HEP.Atlas.Stream

data Sample = Sample { dsid :: Int
                     , totalEventsWeighted :: Int
                     , totalEvents :: Int
                     , events :: Stream Event
                     } deriving (Generic, Show)

instance Binary Sample where
