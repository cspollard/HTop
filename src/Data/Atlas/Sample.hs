{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Atlas.Sample where

import Control.Lens

import GHC.Generics

import Control.Applicative

import GHC.Float

import Data.YODA.Obj
import Data.TTree

data SampleInfo = SampleInfo { dsid :: CInt
                             , totalEvents :: CLong
                             , totalEventsWeighted :: Double
                             } deriving (Show, Generic)


addSampInfo :: SampleInfo -> SampleInfo -> SampleInfo
SampleInfo d t tw `addSampInfo` SampleInfo _ t' tw' = SampleInfo d (t+t') (tw+tw')

-- instance Serialize SampleInfo where

instance FromTTree SampleInfo where
    fromTTree = SampleInfo <$> readBranch "dsid"
                           <*> readBranch "totalEvents"
                           <*> fmap float2Double (readBranch "totalEventsWeighted")


type Sample h = (SampleInfo, h)


-- normalize h to a cross section and drop SampleInfo
-- we can't combine histograms from the same process correctly after
-- this step.
normToXsec :: SampleInfo -> ZipList YodaObj -> ZipList YodaObj
normToXsec si hs = case dsid si of
                        0 -> hs
                        _ -> over (traverse . noted . _H1DD) (`scaledBy` (1.0 / totalEventsWeighted si)) hs
