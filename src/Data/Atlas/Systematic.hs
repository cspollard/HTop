{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Atlas.Systematic where

import Data.Monoid (Product(..))

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
nominal = WeightSystematic "/nominal" $ readWeights ["EvtW", "SFTot"]

pileupUp :: WeightSystematic
pileupUp = WeightSystematic "/pileup_up" $
    do nomW <- readWeights ["EvtW", "SFTot"]
       sfUp <- readWeights ["SFPileUp_UP"]
       sfNom <- readWeights ["SFPileUp"]
       return (nomW * sfUp / sfNom)

pileupDown :: WeightSystematic
pileupDown = WeightSystematic "/pileup_down" $
    do nomW <- readWeights ["EvtW", "SFTot"]
       sfDown <- readWeights ["SFPileUp_DOWN"]
       sfNom <- readWeights ["SFPileUp"]
       return (nomW * sfDown / sfNom)
