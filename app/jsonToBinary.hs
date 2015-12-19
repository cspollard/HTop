{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as BSL

import Data.HEP.Atlas.Stream
import Data.HEP.Atlas.TopTree

evtWeights = ["weight_mc", "weight_pileup", "weight_leptonSF", "weight_bTagSF_77"]
evtSystWeights = ["weight_pileup_UP", "weight_pileup_DOWN",
            "weight_leptonSF_EL_SF_Trigger_UP", "weight_leptonSF_EL_SF_Trigger_DOWN",
            "weight_leptonSF_EL_SF_Reco_UP", "weight_leptonSF_EL_SF_Reco_DOWN",
            "weight_leptonSF_EL_SF_ID_UP", "weight_leptonSF_EL_SF_ID_DOWN",
            "weight_leptonSF_EL_SF_Isol_UP", "weight_leptonSF_EL_SF_Isol_DOWN",
            "weight_leptonSF_MU_SF_Trigger_STAT_UP", "weight_leptonSF_MU_SF_Trigger_STAT_DOWN",
            "weight_leptonSF_MU_SF_Trigger_SYST_UP", "weight_leptonSF_MU_SF_Trigger_SYST_DOWN",
            "weight_leptonSF_MU_SF_ID_STAT_UP", "weight_leptonSF_MU_SF_ID_STAT_DOWN",
            "weight_leptonSF_MU_SF_ID_SYST_UP", "weight_leptonSF_MU_SF_ID_SYST_DOWN"
            ]

main :: IO ()
main = BSL.putStr . encodeList . parseTree evtWeights evtSystWeights =<< BSL.getContents
