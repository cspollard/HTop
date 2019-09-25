{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE OverloadedStrings         #-}


module BFrag.Systematics
  ( recoWgt, trueWgt, treeSysts, lumi
  , DataMC'(..), VarCfg(..)
  , procDict, Reweight1D, Reweight2D
  , svEffSysts, phiResSysts, etaResSysts, ptResSysts, sumPtTrkSysts
  , nsvtrkSyst, npvtrkSyst
  ) where

import Data.Binned
import           Atlas
import           BFrag.Systematics.TrackingSystHists
import           Control.Lens                        (imap)
import           Control.Monad.Writer                hiding ((<>))
import           Data.Bifunctor                      (first)
import qualified Data.IntMap.Strict                  as IM
import           Data.Semigroup                      ((<>))
import qualified Data.Text                           as T
import           Data.TTree
import           GHC.Exts                            (fromList)
import           GHC.Float


-- TODO
-- how can we tell it to only run nominal weight systs on syst trees
-- and data (and no syst trees on data)?
data DataMC' = Data' | MC' VarCfg deriving Show
data VarCfg = NoVars | AllVars deriving Show


lumi :: Vars Double
lumi = Variation 36100 [("LumiUp", 36858.1)]


recoWgt :: (MonadIO m, MonadThrow m) => DataMC' -> TreeRead m (PhysObj ())
recoWgt Data' = return $ pure ()
recoWgt (MC' vcfg) = do
    tw <- trueWgt
    puw <- puWgt vcfg
    jvtw <- jvtWgt vcfg
    lsf <- lepSF vcfg
    bsf <- btagSF vcfg
    return $ tell tw >> puw >> jvtw >> lsf >> bsf


trueWgt :: (MonadThrow m, MonadIO m) => TreeRead m SF
trueWgt = sf "evtw" . float2Double <$> readBranch "weight_mc"


-- TODO
-- partial!
lepSF :: (MonadIO m, MonadThrow m) => VarCfg -> TreeRead m (PhysObj ())
lepSF _ = tell . sf "lepton_sf" . float2Double <$> readBranch "weight_leptonSF"


puWgt :: (MonadIO m, MonadThrow m) => VarCfg -> TreeRead m (PhysObj ())
puWgt vcfg = do
  puw <- float2Double <$> readBranch "weight_pileup"
  case vcfg of
    NoVars -> return . tell $ sf "weight_pileup" puw
    AllVars -> do
      puwup <- float2Double <$> readBranch "weight_pileup_UP"
      puwdown <- float2Double <$> readBranch "weight_pileup_DOWN"
      return . varSF
        $ sf "weight_pileup"
          <$> Variation puw [("puwgtup", puwup), ("puwgtdown", puwdown)]


jvtWgt :: (MonadIO m, MonadThrow m) => VarCfg -> TreeRead m (PhysObj ())
jvtWgt vcfg = do
  jvtw <- float2Double <$> readBranch "weight_jvt"
  case vcfg of
    NoVars -> return . tell $ sf "weight_jvt" jvtw
    AllVars -> do
      jvtwup <- float2Double <$> readBranch "weight_jvt_UP"
      jvtwdown <- float2Double <$> readBranch "weight_jvt_DOWN"
      return . varSF
        $ sf "weight_jvt"
          <$> Variation jvtw [("jvtwgtup", jvtwup), ("jvtwgtdown", jvtwdown)]


btagSF :: (MonadIO m, MonadThrow m) => VarCfg -> TreeRead m (PhysObj ())
btagSF vcfg = do
  btagw <- float2Double <$> readBranch "weight_bTagSF_70"
  case vcfg of
    NoVars -> return . tell $ sf "weight_btag" btagw
    AllVars -> do
      btagwsup <- fmap float2Double <$> readBranch "weight_bTagSF_70_eigenvars_B_up"
      btagwsdown <- fmap float2Double <$> readBranch "weight_bTagSF_70_eigenvars_B_down"
      ctagwsup <- fmap float2Double <$> readBranch "weight_bTagSF_70_eigenvars_C_up"
      ctagwsdown <- fmap float2Double <$> readBranch "weight_bTagSF_70_eigenvars_C_down"
      ltagwsup <- fmap float2Double <$> readBranch "weight_bTagSF_70_eigenvars_Light_up"
      ltagwsdown <- fmap float2Double <$> readBranch "weight_bTagSF_70_eigenvars_Light_down"

      return . varSF . fmap (sf "weight_btag")
        $ Variation btagw
          ( fromList
            $ mconcat
              [ first (<> "up") <$> enum "btagsf" btagwsup
              , first (<> "down") <$> enum "btagsf" btagwsdown
              , first (<> "up") <$> enum "ctagsf" ctagwsup
              , first (<> "down") <$> enum "ctagsf" ctagwsdown
              , first (<> "up") <$> enum "ltagsf" ltagwsup
              , first (<> "down") <$> enum "ltagsf" ltagwsdown
              ]
          )

  where
    enum n = imap (\i x -> (n <> T.pack (show i), x))

treeSysts :: [String]
treeSysts =
  -- [ "JET_21NP_JET_BJES_Response__1down"
  -- , "JET_21NP_JET_BJES_Response__1up"
  -- , "JET_21NP_JET_EffectiveNP_1__1down"
  -- , "JET_21NP_JET_EffectiveNP_1__1up"
  -- , "JET_21NP_JET_EffectiveNP_2__1down"
  -- , "JET_21NP_JET_EffectiveNP_2__1up"
  -- , "JET_21NP_JET_EffectiveNP_3__1down"
  -- , "JET_21NP_JET_EffectiveNP_3__1up"
  -- , "JET_21NP_JET_EffectiveNP_4__1down"
  -- , "JET_21NP_JET_EffectiveNP_4__1up"
  -- , "JET_21NP_JET_EffectiveNP_5__1down"
  -- , "JET_21NP_JET_EffectiveNP_5__1up"
  -- , "JET_21NP_JET_EffectiveNP_6__1down"
  -- , "JET_21NP_JET_EffectiveNP_6__1up"
  -- , "JET_21NP_JET_EffectiveNP_7__1down"
  -- , "JET_21NP_JET_EffectiveNP_7__1up"
  -- , "JET_21NP_JET_EffectiveNP_8restTerm__1down"
  -- , "JET_21NP_JET_EffectiveNP_8restTerm__1up"
  -- , "JET_21NP_JET_EtaIntercalibration_Modelling__1down"
  -- , "JET_21NP_JET_EtaIntercalibration_Modelling__1up"
  -- , "JET_21NP_JET_EtaIntercalibration_NonClosure__1down"
  -- , "JET_21NP_JET_EtaIntercalibration_NonClosure__1up"
  -- , "JET_21NP_JET_EtaIntercalibration_TotalStat__1down"
  -- , "JET_21NP_JET_EtaIntercalibration_TotalStat__1up"
  [ "JET_21NP_JET_Flavor_Composition__1down"
  , "JET_21NP_JET_Flavor_Composition__1up"
  -- , "JET_21NP_JET_Flavor_Response__1down"
  -- , "JET_21NP_JET_Flavor_Response__1up"
  -- , "JET_21NP_JET_Pileup_OffsetMu__1down"
  -- , "JET_21NP_JET_Pileup_OffsetMu__1up"
  -- , "JET_21NP_JET_Pileup_OffsetNPV__1down"
  -- , "JET_21NP_JET_Pileup_OffsetNPV__1up"
  -- , "JET_21NP_JET_Pileup_PtTerm__1down"
  -- , "JET_21NP_JET_Pileup_PtTerm__1up"
  , "JET_21NP_JET_Pileup_RhoTopology__1down"
  , "JET_21NP_JET_Pileup_RhoTopology__1up"
  -- , "JET_21NP_JET_PunchThrough_MC15__1down"
  -- , "JET_21NP_JET_PunchThrough_MC15__1up"
  -- , "JET_21NP_JET_SingleParticle_HighPt__1down"
  -- , "JET_21NP_JET_SingleParticle_HighPt__1up"
  , "JET_JER_SINGLE_NP__1up"
  -- , "EG_RESOLUTION_ALL__1down"
  -- , "EG_RESOLUTION_ALL__1up"
  -- , "EG_SCALE_ALL__1down"
  -- , "EG_SCALE_ALL__1up"
  -- , "MUON_ID__1down"
  -- , "MUON_ID__1up"
  -- , "MUON_MS__1down"
  -- , "MUON_MS__1up"
  -- , "MUON_SAGITTA_RESBIAS__1down"
  -- , "MUON_SAGITTA_RESBIAS__1up"
  -- , "MUON_SAGITTA_RHO__1down"
  -- , "MUON_SAGITTA_RHO__1up"
  -- , "MUON_SCALE__1down"
  -- , "MUON_SCALE__1up"
  ]


procDict :: IM.IntMap T.Text
procDict =
  [ (410000, "PowPy6")
  , (410001, "PowPy6RadUp")
  , (410002, "PowPy6RadDown")
  , (410003, "aMCHerpp")
  , (410004, "PowHerpp")
  , (410501, "PowPy8")
  ]

type Reweight1D = Binned Double Double
type Reweight2D = Binned Double (Binned Double Double)


svEffSysts :: Folder Reweight1D
svEffSysts =
  [ ("v2TRK_BIAS_QOVERP_SAGITTA_WM", sveff_v2TRK_BIAS_QOVERP_SAGITTA_WM)
  , ("v2TRK_EFF_LOOSE_GLOBAL",       sveff_v2TRK_EFF_LOOSE_GLOBAL)
  , ("v2TRK_EFF_LOOSE_IBL",          sveff_v2TRK_EFF_LOOSE_IBL)
  , ("v2TRK_EFF_LOOSE_PHYSMODEL",    sveff_v2TRK_EFF_LOOSE_PHYSMODEL)
  , ("v2TRK_EFF_LOOSE_PP0",          sveff_v2TRK_EFF_LOOSE_PP0)
  , ("v2TRK_EFF_TIGHT_GLOBAL",       sveff_v2TRK_EFF_TIGHT_GLOBAL)
  , ("v2TRK_EFF_TIGHT_IBL",          sveff_v2TRK_EFF_TIGHT_IBL)
  , ("v2TRK_EFF_TIGHT_PHYSMODEL",    sveff_v2TRK_EFF_TIGHT_PHYSMODEL)
  , ("v2TRK_EFF_TIGHT_PP0",          sveff_v2TRK_EFF_TIGHT_PP0)
  , ("v2TRK_FAKE_RATE_LOOSE",        sveff_v2TRK_FAKE_RATE_LOOSE)
  , ("v2TRK_FAKE_RATE_TIGHT",        sveff_v2TRK_FAKE_RATE_TIGHT)
  , ("v2TRK_RES_D0_DEAD",            sveff_v2TRK_RES_D0_DEAD)
  , ("v2TRK_RES_D0_MEAS",            sveff_v2TRK_RES_D0_MEAS)
  , ("v2TRK_RES_Z0_DEAD",            sveff_v2TRK_RES_Z0_DEAD)
  , ("v2TRK_RES_Z0_MEAS",            sveff_v2TRK_RES_Z0_MEAS)
  ]


phiResSysts :: Folder Reweight1D
phiResSysts =
  [ ("v2TRK_BIAS_QOVERP_SAGITTA_WM", svdphi_v2TRK_BIAS_QOVERP_SAGITTA_WM)
  , ("v2TRK_EFF_LOOSE_GLOBAL",       svdphi_v2TRK_EFF_LOOSE_GLOBAL)
  , ("v2TRK_EFF_LOOSE_IBL",          svdphi_v2TRK_EFF_LOOSE_IBL)
  , ("v2TRK_EFF_LOOSE_PHYSMODEL",    svdphi_v2TRK_EFF_LOOSE_PHYSMODEL)
  , ("v2TRK_EFF_LOOSE_PP0",          svdphi_v2TRK_EFF_LOOSE_PP0)
  , ("v2TRK_EFF_TIGHT_GLOBAL",       svdphi_v2TRK_EFF_TIGHT_GLOBAL)
  , ("v2TRK_EFF_TIGHT_IBL",          svdphi_v2TRK_EFF_TIGHT_IBL)
  , ("v2TRK_EFF_TIGHT_PHYSMODEL",    svdphi_v2TRK_EFF_TIGHT_PHYSMODEL)
  , ("v2TRK_EFF_TIGHT_PP0",          svdphi_v2TRK_EFF_TIGHT_PP0)
  , ("v2TRK_FAKE_RATE_LOOSE",        svdphi_v2TRK_FAKE_RATE_LOOSE)
  , ("v2TRK_FAKE_RATE_TIGHT",        svdphi_v2TRK_FAKE_RATE_TIGHT)
  , ("v2TRK_RES_D0_DEAD",            svdphi_v2TRK_RES_D0_DEAD)
  , ("v2TRK_RES_D0_MEAS",            svdphi_v2TRK_RES_D0_MEAS)
  , ("v2TRK_RES_Z0_DEAD",            svdphi_v2TRK_RES_Z0_DEAD)
  , ("v2TRK_RES_Z0_MEAS",            svdphi_v2TRK_RES_Z0_MEAS)
  ]


etaResSysts :: Folder Reweight1D
etaResSysts =
  [ ("v2TRK_BIAS_QOVERP_SAGITTA_WM", svdeta_v2TRK_BIAS_QOVERP_SAGITTA_WM)
  , ("v2TRK_EFF_LOOSE_GLOBAL",       svdeta_v2TRK_EFF_LOOSE_GLOBAL)
  , ("v2TRK_EFF_LOOSE_IBL",          svdeta_v2TRK_EFF_LOOSE_IBL)
  , ("v2TRK_EFF_LOOSE_PHYSMODEL",    svdeta_v2TRK_EFF_LOOSE_PHYSMODEL)
  , ("v2TRK_EFF_LOOSE_PP0",          svdeta_v2TRK_EFF_LOOSE_PP0)
  , ("v2TRK_EFF_TIGHT_GLOBAL",       svdeta_v2TRK_EFF_TIGHT_GLOBAL)
  , ("v2TRK_EFF_TIGHT_IBL",          svdeta_v2TRK_EFF_TIGHT_IBL)
  , ("v2TRK_EFF_TIGHT_PHYSMODEL",    svdeta_v2TRK_EFF_TIGHT_PHYSMODEL)
  , ("v2TRK_EFF_TIGHT_PP0",          svdeta_v2TRK_EFF_TIGHT_PP0)
  , ("v2TRK_FAKE_RATE_LOOSE",        svdeta_v2TRK_FAKE_RATE_LOOSE)
  , ("v2TRK_FAKE_RATE_TIGHT",        svdeta_v2TRK_FAKE_RATE_TIGHT)
  , ("v2TRK_RES_D0_DEAD",            svdeta_v2TRK_RES_D0_DEAD)
  , ("v2TRK_RES_D0_MEAS",            svdeta_v2TRK_RES_D0_MEAS)
  , ("v2TRK_RES_Z0_DEAD",            svdeta_v2TRK_RES_Z0_DEAD)
  , ("v2TRK_RES_Z0_MEAS",            svdeta_v2TRK_RES_Z0_MEAS)
  ]

ptResSysts :: Folder Reweight2D
ptResSysts =
  [ ("v2TRK_BIAS_QOVERP_SAGITTA_WM", svabsres_v2TRK_BIAS_QOVERP_SAGITTA_WM)
  , ("v2TRK_EFF_LOOSE_GLOBAL",       svabsres_v2TRK_EFF_LOOSE_GLOBAL)
  , ("v2TRK_EFF_LOOSE_IBL",          svabsres_v2TRK_EFF_LOOSE_IBL)
  , ("v2TRK_EFF_LOOSE_PHYSMODEL",    svabsres_v2TRK_EFF_LOOSE_PHYSMODEL)
  , ("v2TRK_EFF_LOOSE_PP0",          svabsres_v2TRK_EFF_LOOSE_PP0)
  , ("v2TRK_EFF_TIGHT_GLOBAL",       svabsres_v2TRK_EFF_TIGHT_GLOBAL)
  , ("v2TRK_EFF_TIGHT_IBL",          svabsres_v2TRK_EFF_TIGHT_IBL)
  , ("v2TRK_EFF_TIGHT_PHYSMODEL",    svabsres_v2TRK_EFF_TIGHT_PHYSMODEL)
  , ("v2TRK_EFF_TIGHT_PP0",          svabsres_v2TRK_EFF_TIGHT_PP0)
  , ("v2TRK_FAKE_RATE_LOOSE",        svabsres_v2TRK_FAKE_RATE_LOOSE)
  , ("v2TRK_FAKE_RATE_TIGHT",        svabsres_v2TRK_FAKE_RATE_TIGHT)
  , ("v2TRK_RES_D0_DEAD",            svabsres_v2TRK_RES_D0_DEAD)
  , ("v2TRK_RES_D0_MEAS",            svabsres_v2TRK_RES_D0_MEAS)
  , ("v2TRK_RES_Z0_DEAD",            svabsres_v2TRK_RES_Z0_DEAD)
  , ("v2TRK_RES_Z0_MEAS",            svabsres_v2TRK_RES_Z0_MEAS)
  ]

sumPtTrkSysts :: Folder Reweight2D
sumPtTrkSysts =
  [ ("v2TRK_BIAS_QOVERP_SAGITTA_WM", trkptsum_v2TRK_BIAS_QOVERP_SAGITTA_WM)
  , ("v2TRK_EFF_LOOSE_GLOBAL",       trkptsum_v2TRK_EFF_LOOSE_GLOBAL)
  , ("v2TRK_EFF_LOOSE_IBL",          trkptsum_v2TRK_EFF_LOOSE_IBL)
  , ("v2TRK_EFF_LOOSE_PHYSMODEL",    trkptsum_v2TRK_EFF_LOOSE_PHYSMODEL)
  , ("v2TRK_EFF_LOOSE_PP0",          trkptsum_v2TRK_EFF_LOOSE_PP0)
  , ("v2TRK_EFF_TIGHT_GLOBAL",       trkptsum_v2TRK_EFF_TIGHT_GLOBAL)
  , ("v2TRK_EFF_TIGHT_IBL",          trkptsum_v2TRK_EFF_TIGHT_IBL)
  , ("v2TRK_EFF_TIGHT_PHYSMODEL",    trkptsum_v2TRK_EFF_TIGHT_PHYSMODEL)
  , ("v2TRK_EFF_TIGHT_PP0",          trkptsum_v2TRK_EFF_TIGHT_PP0)
  , ("v2TRK_FAKE_RATE_LOOSE",        trkptsum_v2TRK_FAKE_RATE_LOOSE)
  , ("v2TRK_FAKE_RATE_TIGHT",        trkptsum_v2TRK_FAKE_RATE_TIGHT)
  , ("v2TRK_RES_D0_DEAD",            trkptsum_v2TRK_RES_D0_DEAD)
  , ("v2TRK_RES_D0_MEAS",            trkptsum_v2TRK_RES_D0_MEAS)
  , ("v2TRK_RES_Z0_DEAD",            trkptsum_v2TRK_RES_Z0_DEAD)
  , ("v2TRK_RES_Z0_MEAS",            trkptsum_v2TRK_RES_Z0_MEAS)
  ]

nsvtrkSyst :: Reweight1D
nsvtrkSyst = Binned xbins . IM.fromList $ enumerate vals
  where
    xbins = [3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13]
    vals =
      [ 9.628969e-01, 9.738253e-01, 1.018527e+00, 1.076142e+00, 1.115221e+00
      , 1.108247e+00, 9.983492e-01, 1.028850e+00, 6.806861e-01, 1.400987e+00
      ]

npvtrkSyst :: Reweight1D
npvtrkSyst = Binned xbins . IM.fromList $ enumerate vals
  where
    xbins = [0..20]
    vals =
      [ 8.905407e-01, 9.322888e-01, 1.011352e+00, 1.031504e+00, 9.908142e-01
      , 1.006516e+00, 1.034857e+00, 1.014147e+00, 1.010799e+00, 9.657193e-01
      , 1.015290e+00, 9.757830e-01, 1.013545e+00, 9.402477e-01, 9.822957e-01
      , 9.720444e-01, 9.011401e-01, 9.163709e-01, 9.912843e-01, 1.039900e+00
      ]
