{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Atlas.Sample where

import Control.Lens

import Data.Serialize
import GHC.Generics

import Control.Applicative

import Foreign.C.Types (CLong)

import Data.Histogram.Extra

import Data.TTree

import GHC.Float

data SampleInfo = SampleInfo { dsid :: Int
                             , totalEvents :: CLong
                             , totalEventsWeighted :: Double
                             } deriving (Show, Generic)


-- instance Serialize SampleInfo where

instance FromTTree SampleInfo where
    fromTTree = SampleInfo <$> readBranch "dsid"
                           <*> readBranch "totalEvents"
                           <*> fmap float2Double (readBranch "totalEventsWeighted")


type Sample h = (SampleInfo, h)


-- normalize h to a cross section and drop SampleInfo
-- we can't combine histograms from the same process correctly after
-- this step.
normToXsec :: SampleInfo -> ZipList YodaHisto1D -> ZipList YodaHisto1D
normToXsec si hs = case dsid si of
                        0 -> hs
                        _ -> fmap (over yhHisto (`scaleBy` (1.0 / totalEventsWeighted si))) hs
