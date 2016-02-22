{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Parallel.Strategies (using, rseq, parBuffer)
import qualified Data.ByteString.Lazy as BSL

import Data.Maybe (fromJust)
import Control.Arrow ((&&&))

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


evtWeights = ["weight_mc", "weight_pileup", "weight_leptonSF"]
evtSystWeights = ["weight_pileup_UP", "weight_pileup_DOWN",
                  "weight_leptonSF_EL_SF_Trigger_UP", "weight_leptonSF_EL_SF_Trigger_DOWN",
                  "weight_leptonSF_EL_SF_Reco_UP", "weight_leptonSF_EL_SF_Reco_DOWN",
                  "weight_leptonSF_EL_SF_ID_UP", "weight_leptonSF_EL_SF_ID_DOWN",
                  "weight_leptonSF_EL_SF_Isol_UP", "weight_leptonSF_EL_SF_Isol_DOWN",
                  "weight_leptonSF_MU_SF_Trigger_STAT_UP", "weight_leptonSF_MU_SF_Trigger_STAT_DOWN",
                  "weight_leptonSF_MU_SF_Trigger_SYST_UP", "weight_leptonSF_MU_SF_Trigger_SYST_DOWN",
                  "weight_leptonSF_MU_SF_ID_STAT_UP", "weight_leptonSF_MU_SF_ID_STAT_DOWN",
                  "weight_leptonSF_MU_SF_ID_SYST_UP", "weight_leptonSF_MU_SF_ID_SYST_DOWN",
                  "weight_leptonSF_MU_SF_Isol_STAT_UP", "weight_leptonSF_MU_SF_Isol_STAT_DOWN",
                  "weight_leptonSF_MU_SF_Isol_SYST_UP", "weight_leptonSF_MU_SF_Isol_SYST_DOWN",
                  "weight_leptonSF_MU_SF_TTVA_STAT_UP", "weight_leptonSF_MU_SF_TTVA_STAT_DOWN",
                  "weight_leptonSF_MU_SF_TTVA_SYST_UP", "weight_leptonSF_MU_SF_TTVA_SYST_DOWN",
                  "weight_indiv_SF_EL_Trigger_UP", "weight_indiv_SF_EL_Trigger_DOWN",
                  "weight_indiv_SF_EL_Reco_UP", "weight_indiv_SF_EL_Reco_DOWN",
                  "weight_indiv_SF_EL_ID_UP", "weight_indiv_SF_EL_ID_DOWN",
                  "weight_indiv_SF_EL_Isol_UP", "weight_indiv_SF_EL_Isol_DOWN",
                  "weight_indiv_SF_MU_Trigger_STAT_UP", "weight_indiv_SF_MU_Trigger_STAT_DOWN",
                  "weight_indiv_SF_MU_Trigger_SYST_UP", "weight_indiv_SF_MU_Trigger_SYST_DOWN",
                  "weight_indiv_SF_MU_ID_STAT_UP", "weight_indiv_SF_MU_ID_STAT_DOWN",
                  "weight_indiv_SF_MU_ID_SYST_UP", "weight_indiv_SF_MU_ID_SYST_DOWN",
                  "weight_indiv_SF_MU_Isol_STAT_UP", "weight_indiv_SF_MU_Isol_STAT_DOWN",
                  "weight_indiv_SF_MU_Isol_SYST_UP", "weight_indiv_SF_MU_Isol_SYST_DOWN",
                  "weight_indiv_SF_MU_TTVA_STAT_UP", "weight_indiv_SF_MU_TTVA_STAT_DOWN",
                  "weight_indiv_SF_MU_TTVA_SYST_UP", "weight_indiv_SF_MU_TTVA_SYST_DOWN"
                  ]

otherWeights = ["weight_indiv_SF_MU_TTVA",
                "weight_indiv_SF_MU_Isol",
                "weight_indiv_SF_MU_ID",
                "weight_indiv_SF_MU_Trigger",
                "weight_indiv_SF_EL_Trigger",
                "weight_indiv_SF_EL_Reco",
                "weight_indiv_SF_EL_ID",
                "weight_indiv_SF_EL_Isol"
                ]


main :: IO ()
main = do
        evts <- liftM (parseTree evtWeights evtSystWeights) BSL.getContents :: IO Events

        let hists = concatMap concat $ built $ feedl' (eventSystHists ("nominal" : evtSystWeights)) $ using (take 5000 evts) (parBuffer 8 rseq)
        BSL.putStr . encodeList $ hists


-- example cuts
minPt :: HasLorentzVector a => Double -> Cut a
minPt x = (> x) . lvPt . toPtEtaPhiE

maxAbsEta :: HasLorentzVector a => Double -> Cut a
maxAbsEta x = (< x) . abs . lvEta . toPtEtaPhiE

minMV2c20 :: Double -> Cut Jet
minMV2c20 x = (> x) . jMV2c20

nBtags :: Event -> Int
nBtags = nJets $ minPt 25000 `cAnd` maxAbsEta 2.5 `cAnd` minMV2c20 0.7
