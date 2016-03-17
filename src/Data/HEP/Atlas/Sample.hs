{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Data.HEP.Atlas.Sample where

import Data.Serialize
import GHC.Generics
import Data.Aeson
import Data.Monoid

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
instance Monoid SampleInfo where
    s `mappend` s' = SampleInfo
                        (dsid s')
                        (numEvents s + numEvents s')
                        (sumWeights s + sumWeights s')

    mempty = SampleInfo 0 0 0
