{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Atlas.Systematic where

import Data.Monoid (Product(..), (<>))

import Data.Text
import GHC.Float

import Data.TTree

data WeightSystematic =
    WeightSystematic
        { systName :: Text
        , readWeight :: forall m. MonadIO m => TR m Double
        }

instance Show WeightSystematic where
    show = unpack . systName


readWeights :: MonadIO m => [Text] -> TR m Double
readWeights ws = float2Double . getProduct . foldMap Product
                        <$> mapM (readBranch . unpack) ws


nominal :: WeightSystematic
nominal = WeightSystematic "nominal" $ readWeights ["SFTot"]

systUp :: Text -> [Text] -> WeightSystematic
systUp n ws = WeightSystematic (n <> "_up") $ readWeights ws

systDown :: Text -> [Text] -> WeightSystematic
systDown n ws = WeightSystematic (n <> "_down") $ readWeights ws
