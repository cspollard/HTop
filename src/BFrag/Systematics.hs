{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeFamilies              #-}


module BFrag.Systematics
  ( evtWgt, ttbarSysts, treeSysts, lumi
  , DataMC'(..), VarCfg(..)
  ) where

import           Atlas
import           Control.Arrow  (first)
import           Control.Lens
import qualified Data.IntMap    as IM
import qualified Data.Map       as M
import           Data.Semigroup
import qualified Data.Text      as T
import           Data.TTree
import           GHC.Float


type ProcMap = IM.IntMap

-- TODO
-- how can we tell it to only run nominal weight systs on syst trees
-- and data (and no syst trees on data)?
data DataMC' = Data' | MC' VarCfg deriving Show
data VarCfg = NoVars | AllVars deriving Show

lumi :: Vars Double
lumi = Variations 38000 [("LumiUp", 41800), ("LumiDown", 34200)]

evtWgt :: (MonadFail m, MonadIO m) => DataMC' -> TreeRead m (Vars SF)
evtWgt Data' = return mempty
evtWgt (MC' vcfg) = do
    pu <- puWgt vcfg
    liftIO $ print pu
    jvt <- jvtWgt vcfg
    liftIO $ print jvt
    lsf <- lepSF vcfg
    liftIO $ print lsf
    evtw <- pure . sf "evtw" . float2Double <$> readBranch "weight_mc"
    return $ evtw <> pu <> lsf <> jvt


-- TODO
-- partial!
lepSF :: (MonadIO m, MonadFail m) => VarCfg -> TreeRead m (Vars SF)
lepSF _ = pure . sf "lepsf" . float2Double <$> readBranch "weight_leptonSF"
-- TODO
-- in XRedTop there are 38 (!!) lepSF variations. This can't be right.
-- lepSF NoVars = pure . sf "lepsf" . float2Double . head <$> readBranch "SFLept"
-- lepSF AllVars = do
--   (nom:vars) <- fmap float2Double <$> readBranch "SFLept"
--   let vars' =
--         M.fromList
--         . imap (\i x -> ("lepsf" <> T.pack (show i), sf "lepsf" x))
--         $ vars
--
--   return $ Variations (sf "lepsf" nom) vars'

puWgt :: (MonadIO m, MonadFail m) => VarCfg -> TreeRead m (Vars SF)
puWgt vcfg = do
  puw <- float2Double <$> readBranch "weight_pileup"
  case vcfg of
    NoVars -> return . fmap (sf "puwgt") $ pure puw
    AllVars -> do
      puwup <- float2Double <$> readBranch "weight_pileup_UP"
      puwdown <- float2Double <$> readBranch "weight_pileup_DOWN"
      return . fmap (sf "puwgt") . Variations puw
        $ [("puwgtup", puwup), ("puwgtdown", puwdown)]


jvtWgt :: (MonadIO m, MonadFail m) => VarCfg -> TreeRead m (Vars SF)
jvtWgt vcfg = do
  jvtw <- float2Double <$> readBranch "weight_jvt"
  case vcfg of
    NoVars -> return . fmap (sf "jvtwgt") $ pure jvtw
    AllVars -> do
      jvtwup <- float2Double <$> readBranch "weight_jvt_UP"
      jvtwdown <- float2Double <$> readBranch "weight_jvt_DOWN"
      return . fmap (sf "jvtwgt") . Variations jvtw
        $ [("jvtwgtup", jvtwup), ("jvtwgtdown", jvtwdown)]


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
        foldr
          ( \(k, fs) fv ->
              inF2 (M.intersectionWith (\s v -> v & variations . at k ?~ s)) fs fv
          )
          x
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
