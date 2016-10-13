{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Atlas.Systematic where

import Data.Monoid (Product(..))

import Data.Text
import GHC.Float

import Data.TTree

-- TODO
-- other types of systs...
data Systematic =
    WeightSystematic
        { systName :: Text
        , readWeight :: forall m. MonadIO m => TR m Double
        }

instance Show Systematic where
    show = show . systName


nominal :: Systematic
nominal = WeightSystematic "nominal" $ readWeights ["EvtW", "SFTot"]

pileupUp :: Systematic
pileupUp = WeightSystematic "pileup_up" $
    do nomW <- readWeights ["EvtW", "SFTot"]
       sfUp <- readWeights ["SFPileUp_UP"]
       sfNom <- readWeights ["SFPileUp"]
       return $ if sfNom == 0 then 0 else nomW * sfUp / sfNom

pileupDown :: Systematic
pileupDown = WeightSystematic "pileup_down" $
    do nomW <- readWeights ["EvtW", "SFTot"]
       sfDown <- readWeights ["SFPileUp_DOWN"]
       sfNom <- readWeights ["SFPileUp"]
       return $ if sfNom == 0 then 0 else nomW * sfDown / sfNom


readWeights :: MonadIO m => [Text] -> TR m Double
readWeights ws = float2Double . getProduct . foldMap Product
                        <$> mapM (readBranch . unpack) ws
