{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as BSL

import Data.Maybe (fromJust)

import Data.HEP.Atlas.TopTree
import Data.HEP.Atlas.Event
import Data.HEP.Atlas.Jet
import Data.HEP.Cut
import Data.HEP.LorentzVector

import Control.Monad (forM_, liftM)
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Monoid

import qualified Data.Vector as V
import Data.Uncertain

import Data.Histogram
import Data.HEP.Atlas.Histograms
import Data.HEP.Atlas.Stream

minPt :: HasLorentzVector a => Double -> Cut a
minPt x = (> x) . lvPt . toPtEtaPhiE

maxAbsEta :: HasLorentzVector a => Double -> Cut a
maxAbsEta x = (< x) . abs . lvEta . toPtEtaPhiE

minMV2c20 :: Double -> Cut Jet
minMV2c20 x = (> x) . jMV2c20

nBtags :: Event -> Int
nBtags = nJets $ minPt 25000 `cAnd` maxAbsEta 2.5 `cAnd` minMV2c20 0.7


evtWeights = ["weight_mc", "weight_pileup", "weight_leptonSF", "weight_bTagSF_77"]
evtSystWeights = ["weight_pileup_UP", "weight_pileup_DOWN"] {-,
            "weight_leptonSF_EL_SF_Trigger_UP", "weight_leptonSF_EL_SF_Trigger_DOWN",
            "weight_leptonSF_EL_SF_Reco_UP", "weight_leptonSF_EL_SF_Reco_DOWN",
            "weight_leptonSF_EL_SF_ID_UP", "weight_leptonSF_EL_SF_ID_DOWN",
            "weight_leptonSF_EL_SF_Isol_UP", "weight_leptonSF_EL_SF_Isol_DOWN",
            "weight_leptonSF_MU_SF_Trigger_STAT_UP", "weight_leptonSF_MU_SF_Trigger_STAT_DOWN",
            "weight_leptonSF_MU_SF_Trigger_SYST_UP", "weight_leptonSF_MU_SF_Trigger_SYST_DOWN",
            "weight_leptonSF_MU_SF_ID_STAT_UP", "weight_leptonSF_MU_SF_ID_STAT_DOWN",
            "weight_leptonSF_MU_SF_ID_SYST_UP", "weight_leptonSF_MU_SF_ID_SYST_DOWN"
            ]
            -}


main :: IO ()
main = do
        evts <- liftM (parseTree evtWeights evtSystWeights) BSL.getContents :: IO Events

        let hists = concatMap concat $ built $ feedr' (eventSystHists ("nominal" : evtSystWeights)) evts
        BSL.putStr . encodeList $ hists
