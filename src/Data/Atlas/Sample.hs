{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Atlas.Sample where

import Data.Serialize
import GHC.Generics
import Data.Aeson

import Data.Semigroup

import Data.Histogram.Funcs

data SampleInfo = SampleInfo { dsid :: Int
                             , numEvents :: Int
                             , sumWeights :: Double
                             } deriving (Show, Generic)

instance Serialize SampleInfo where

instance FromJSON SampleInfo where
    parseJSON = withObject "cannot parse SampleInfo." $
                    \v -> SampleInfo
                        <$> v .: "dsid"
                        <*> v .: "totalEvents"
                        <*> v .: "totalEventsWeighted"

-- TODO
-- dsids should not added...
instance Semigroup SampleInfo where
    s <> s' = SampleInfo
                (dsid s')
                (numEvents s + numEvents s')
                (sumWeights s + sumWeights s')


type Sample h = (SampleInfo, [h])

-- normalize h to a cross section and drop SampleInfo
-- we can't combine histograms from the same process correctly after
-- this step.
freezeSample :: Sample YodaHisto1D -> [YodaHisto1D]
freezeSample (si, hs) = case dsid si of
                                0 -> hs
                                _ -> map (`scaleBy` (1.0 / sumWeights si)) hs
