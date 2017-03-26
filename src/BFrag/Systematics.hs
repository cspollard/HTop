{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeFamilies              #-}


module BFrag.Systematics
  ( evtWgt, ttbarSysts, treeSysts
  , DataMC'(..), VarCfg(..)
  ) where

import           Atlas
import           Control.Arrow (first)
import           Control.Lens
import qualified Data.IntMap   as IM
import qualified Data.Map      as M
import           Data.Monoid
import qualified Data.Text     as T
import           Data.TTree
import           GHC.Float


-- TODO
-- how can we tell it to only run nominal weight systs on syst trees
-- and data (and no syst trees on data)?

type ProcMap = IM.IntMap

data DataMC' = Data' | MC' VarCfg
data VarCfg = NoVars | AllVars

evtWgt :: MonadIO m => DataMC' -> TR m (Vars SF)
evtWgt Data' = return mempty
evtWgt (MC' vcfg) = do
    pu <- puWgt vcfg
    lsf <- lepSF vcfg
    evtw <-
      fmap (pure . sf "evtw" . float2Double . product) . traverse readBranch
        $ (["EvtW", "SFZVtx", "SFJVT"] :: [String])
    return $ evtw <> pu <> lsf


-- TODO
-- Partial!
lepSF :: MonadIO m => VarCfg -> TR m (Vars SF)
lepSF NoVars = pure . sf "lepsf" . float2Double . head <$> readBranch "SFLept"
lepSF AllVars = pure . sf "lepsf" . float2Double . head <$> readBranch "SFLept"
-- TODO
-- in XRedTop there are 38 (!!) lepSF variations. This can't be right.
-- lepSF = do
--   (nom:vars) <- fmap float2Double <$> readBranch "SFLept"
--   let vars' =
--         M.fromList
--         . imap (\i x -> ("lepsf" <> T.pack (show i), sf "lepsf" x))
--         $ vars
--
--   return $ Variations (sf "lepsf" nom) vars'

puWgt :: MonadIO m => VarCfg -> TR m (Vars SF)
puWgt vcfg = do
  puw <- float2Double <$> readBranch "SFPileUp"
  case vcfg of
    NoVars -> return . fmap (sf "pileupwgt") $ pure puw
    AllVars -> do
      puwup <- float2Double <$> readBranch "SFPileUp_UP"
      puwdown <- float2Double <$> readBranch "SFPileUp_DOWN"
      return . fmap (sf "pileupwgt") . Variations puw
        $ [("puwup", puwup), ("puwdown", puwdown)]


treeSysts :: [String]
treeSysts =
  [ "JET_21NP_JET_BJES_Response__1down"
  , "JET_21NP_JET_BJES_Response__1up"
  , "JET_21NP_JET_EffectiveNP_1__1down"
  , "JET_21NP_JET_EffectiveNP_1__1up"
  , "JET_21NP_JET_EffectiveNP_2__1down"
  , "JET_21NP_JET_EffectiveNP_2__1up"
  , "JET_21NP_JET_EffectiveNP_3__1down"
  , "JET_21NP_JET_EffectiveNP_3__1up"
  , "JET_21NP_JET_EffectiveNP_4__1down"
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
  -- , "JET_21NP_JET_Flavor_Composition__1down"
  -- , "JET_21NP_JET_Flavor_Composition__1up"
  -- , "JET_21NP_JET_Flavor_Response__1down"
  -- , "JET_21NP_JET_Flavor_Response__1up"
  -- , "JET_21NP_JET_Pileup_OffsetMu__1down"
  -- , "JET_21NP_JET_Pileup_OffsetMu__1up"
  -- , "JET_21NP_JET_Pileup_OffsetNPV__1down"
  -- , "JET_21NP_JET_Pileup_OffsetNPV__1up"
  -- , "JET_21NP_JET_Pileup_PtTerm__1down"
  -- , "JET_21NP_JET_Pileup_PtTerm__1up"
  -- , "JET_21NP_JET_Pileup_RhoTopology__1down"
  -- , "JET_21NP_JET_Pileup_RhoTopology__1up"
  -- , "JET_21NP_JET_PunchThrough_MC15__1down"
  -- , "JET_21NP_JET_PunchThrough_MC15__1up"
  -- , "JET_21NP_JET_SingleParticle_HighPt__1down"
  -- , "JET_21NP_JET_SingleParticle_HighPt__1up"
  -- , "JET_JER_SINGLE_NP__1up"
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


ttbarSysts
  :: ProcMap (Folder (Vars YodaObj)) -> ProcMap (Folder (Vars YodaObj))
ttbarSysts preds =
  let systttbarDSIDs = (+410000) <$> [1, 2, 3, 4] :: [Int]
      (systttbar', systpreds) =
        IM.partitionWithKey (\k _ -> k `elem` systttbarDSIDs) preds

      -- the "nominal" variations from the ttbar systematic samples
      -- are actually variations
      systttbar = over (traverse.traverse) (view nominal) systttbar'

      addSysts x =
        foldr (\(k, fs) fv ->
          inF2 (M.intersectionWith (\s v -> v & at k ?~ s)) fs fv) x
          $ fmap (first (procDict IM.!))
            . IM.toList
            $ systttbar

  in over (ix 410000) addSysts systpreds


procDict :: IM.IntMap T.Text
procDict =
  [ (410000, "PowPyNom")
  , (410001, "PowPyRadUp")
  , (410002, "PowPyRadDown")
  , (410003, "aMCHer")
  , (410004, "PowHer")
  ]
