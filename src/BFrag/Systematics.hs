{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}


module BFrag.Systematics
  ( recoWgt, trueWgt, treeSysts, lumi
  , DataMC'(..), VarCfg(..)
  , procDict
  ) where

import           Atlas
import qualified Data.IntMap.Strict as IM
import qualified Data.Text          as T
import           Data.TTree
import           GHC.Float


-- TODO
-- how can we tell it to only run nominal weight systs on syst trees
-- and data (and no syst trees on data)?
data DataMC' = Data' | MC' VarCfg deriving Show
data VarCfg = NoVars | AllVars deriving Show

lumi :: Vars Double
lumi = Variation 38000 [("LumiUp", 41800), ("LumiDown", 34200)]

recoWgt :: (MonadIO m, MonadThrow m) => DataMC' -> TreeRead m (PhysObj ())
recoWgt Data' = return $ pure ()
recoWgt (MC' vcfg) = do
    tw <- trueWgt
    puw <- puWgt vcfg
    jvtw <- jvtWgt vcfg
    lsf <- lepSF vcfg
    return $ tw >> puw >> jvtw >> lsf


trueWgt :: (MonadThrow m, MonadIO m) => TreeRead m (PhysObj ())
trueWgt = dictate . sf "evtw" . float2Double <$> readBranch "weight_mc"


-- TODO
-- partial!
lepSF :: (MonadIO m, MonadThrow m) => VarCfg -> TreeRead m (PhysObj ())
lepSF _ = dictate . sf "lepton_sf" . float2Double <$> readBranch "weight_leptonSF"
-- TODO
-- in XRedTop there are 38 (!!) lepSF variations. This can't be right.
-- lepSF NoVarsT m = pure . sf "lepsf" . float2Double . head <$> readBranch "SFLept"
-- lepSF AllVarsT m = do
--   (nom:vars) <- fmap float2Double <$> readBranch "SFLept"
--   let vars' =
--         M.fromList
--         . imap (\i x -> ("lepsf" <> T.pack (show i), sf "lepsf" x))
--         $ vars
--
--   return $ Variations (sf "lepsf" nom) vars'

puWgt :: (MonadIO m, MonadThrow m) => VarCfg -> TreeRead m (PhysObj ())
puWgt vcfg = do
  puw <- float2Double <$> readBranch "weight_pileup"
  case vcfg of
    NoVars -> return . dictate $ sf "weight_pileup" puw
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
    NoVars -> return . dictate $ sf "weight_jvt" jvtw
    AllVars -> do
      jvtwup <- float2Double <$> readBranch "weight_jvt_UP"
      jvtwdown <- float2Double <$> readBranch "weight_jvt_DOWN"
      return . varSF
        $ sf "weight_jvt"
          <$> Variation jvtw [("jvtwgtup", jvtwup), ("jvtwgtdown", jvtwdown)]


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
  , "JET_21NP_JET_EffectiveNP_4__1up"
  , "JET_21NP_JET_EffectiveNP_5__1down"
  , "JET_21NP_JET_EffectiveNP_5__1up"
  , "JET_21NP_JET_EffectiveNP_6__1down"
  , "JET_21NP_JET_EffectiveNP_6__1up"
  , "JET_21NP_JET_EffectiveNP_7__1down"
  , "JET_21NP_JET_EffectiveNP_7__1up"
  , "JET_21NP_JET_EffectiveNP_8restTerm__1down"
  , "JET_21NP_JET_EffectiveNP_8restTerm__1up"
  , "JET_21NP_JET_EtaIntercalibration_Modelling__1down"
  , "JET_21NP_JET_EtaIntercalibration_Modelling__1up"
  , "JET_21NP_JET_EtaIntercalibration_NonClosure__1down"
  , "JET_21NP_JET_EtaIntercalibration_NonClosure__1up"
  , "JET_21NP_JET_EtaIntercalibration_TotalStat__1down"
  , "JET_21NP_JET_EtaIntercalibration_TotalStat__1up"
  , "JET_21NP_JET_Flavor_Composition__1down"
  , "JET_21NP_JET_Flavor_Composition__1up"
  , "JET_21NP_JET_Flavor_Response__1down"
  , "JET_21NP_JET_Flavor_Response__1up"
  , "JET_21NP_JET_Pileup_OffsetMu__1down"
  , "JET_21NP_JET_Pileup_OffsetMu__1up"
  , "JET_21NP_JET_Pileup_OffsetNPV__1down"
  , "JET_21NP_JET_Pileup_OffsetNPV__1up"
  , "JET_21NP_JET_Pileup_PtTerm__1down"
  , "JET_21NP_JET_Pileup_PtTerm__1up"
  , "JET_21NP_JET_Pileup_RhoTopology__1down"
  , "JET_21NP_JET_Pileup_RhoTopology__1up"
  , "JET_21NP_JET_PunchThrough_MC15__1down"
  , "JET_21NP_JET_PunchThrough_MC15__1up"
  , "JET_21NP_JET_SingleParticle_HighPt__1down"
  , "JET_21NP_JET_SingleParticle_HighPt__1up"
  , "JET_JER_SINGLE_NP__1up"
  , "EG_RESOLUTION_ALL__1down"
  , "EG_RESOLUTION_ALL__1up"
  , "EG_SCALE_ALL__1down"
  , "EG_SCALE_ALL__1up"
  , "MUON_ID__1down"
  , "MUON_ID__1up"
  , "MUON_MS__1down"
  , "MUON_MS__1up"
  , "MUON_SAGITTA_RESBIAS__1down"
  , "MUON_SAGITTA_RESBIAS__1up"
  , "MUON_SAGITTA_RHO__1down"
  , "MUON_SAGITTA_RHO__1up"
  , "MUON_SCALE__1down"
  , "MUON_SCALE__1up"
  ]


-- TODO
-- is this partial?
-- ttbarSysts
--   :: ProcMap (Folder (Vars YodaObj)) -> ProcMap (Folder (Vars YodaObj))
-- ttbarSysts preds =
--   let systttbarDSIDs = (+410000) <$> [1, 2, 3, 4] :: [Int]
--       (systttbar', systpreds) =
--         IM.partitionWithKey (\k _ -> k `elem` systttbarDSIDs) preds
--
--       -- the "nominal" variations from the ttbar systematic samples
--       -- are actually variations
--       systttbar = over (traverse.traverse) (view nominal) systttbar'
--
--       addSysts x =
--         foldr
--           ( \(k, fs) fv ->
--               inF2 (M.intersectionWith (\s v -> v & variations . at k ?~ s)) fs fv
--           )
--           x
--           $ fmap (first (procDict IM.!))
--             . IM.toList
--             $ systttbar
--
--   in over (ix 410000) addSysts systpreds


procDict :: IM.IntMap T.Text
procDict =
  [ (410000, "PowPy6")
  , (410001, "PowPy6RadUp")
  , (410002, "PowPy6RadDown")
  , (410003, "aMCHerpp")
  , (410004, "PowHerpp")
  , (410501, "PowPy8")
  ]
