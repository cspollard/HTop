{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Data.Atlas.TopTree where

import Data.List (sortBy)
import Data.Ord (comparing, Down(..))
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

import Control.Applicative
import Data.Vector (Vector)
import qualified Data.Vector as V

import Data.Text (Text, unpack)
import Data.Aeson (Value(..), withObject, (.:), FromJSON(..))
import Data.Aeson.Types (Parser)

import Data.Semigroup
import Control.Monad (forM)

import Data.HEP.LorentzVector
import Data.Atlas.Event
import Data.Atlas.Helpers

import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Serialize
-- import Data.Serialize.Get
import Data.ByteString.Char8 (ByteString)


parseBranch :: FromJSON a => Text -> Value -> Parser a
parseBranch name = withObject
                        ("parseBranch: the item with key " <> unpack name <> " is not an object.")
                        (.: name)


zipWithA :: (Applicative m) => m (Vector (a -> b)) -> m (Vector a) -> m (Vector b)
zipWithA = liftA2 (V.zipWith ($))


parsePtEtaPhiEs :: Text -> Value -> Parser (Vector PtEtaPhiE)
parsePtEtaPhiEs prefix val = fmap PtEtaPhiE `fmap`
                                parseBranch (prefix <> "pt") val `zipWithA`
                                parseBranch (prefix <> "eta") val `zipWithA`
                                parseBranch (prefix <> "phi") val `zipWithA`
                                parseBranch (prefix <> "e") val


parseElectrons :: Value -> Parser Electrons
parseElectrons val = fmap Electron `fmap`
                        parsePtEtaPhiEs "el_" val `zipWithA`
                        parseBranch "el_cl_eta" val `zipWithA`
                        parseBranch "el_charge" val `zipWithA`
                        parseBranch "el_d0sig" val `zipWithA`
                        parseBranch "el_ptvarcone20" val



parseMuons :: Value -> Parser Muons
parseMuons val = fmap Muon `fmap`
                    parsePtEtaPhiEs "mu_" val `zipWithA`
                    parseBranch "mu_charge" val `zipWithA`
                    parseBranch "mu_d0sig" val `zipWithA`
                    parseBranch "mu_ptvarcone30" val


parseJets :: Value -> Parser Jets
parseJets val = fmap Jet `fmap`
                    parsePtEtaPhiEs "jet_" val `zipWithA`
                    parseBranch "jet_mv2c20" val `zipWithA`
                    parseBranch "jet_jvt" val


parseLargeJets :: Value -> Parser LargeJets
parseLargeJets val = fmap LargeJet `fmap`
                        parsePtEtaPhiEs "ljet_" val `zipWithA`
                        parseBranch "ljet_m" val `zipWithA`
                        parseBranch "ljet_sd12" val `zipWithA`
                        parseBranch "ljet_tau21" val `zipWithA`
                        parseBranch "ljet_tau32" val `zipWithA`
                        parseBranch "ljet_ghosttrackjet_idx" val


parseTrackJets :: Value -> Parser TrackJets
parseTrackJets val = fmap TrackJet `fmap`
                        parsePtEtaPhiEs "tjet_" val `zipWithA`
                        parseBranch "tjet_mv2c20" val `zipWithA`
                        (sequenceA <$> parseBranch "tjet_label" val)


parseMET :: Value -> Parser PtEtaPhiE
parseMET val = do et <- parseBranch "met_met" val
                  flip (PtEtaPhiE et 0) et <$> parseBranch "met_phi" val


ptSort :: HasLorentzVector v => Vector v -> Vector v
ptSort = V.fromList . sortBy (comparing (Down . lvPt . toPtEtaPhiE)) . V.toList


parseBranchMap :: FromJSON v => [Text] -> Value -> Parser (M.Map Text v)
parseBranchMap ts v = M.fromList <$> forM ts (\t -> (,) t <$> parseBranch t v)



-- TODO
-- orphan instance...
-- but I have to move all functions above to Event, or move below
-- functions out of TopTree, otherwise imports will break.
-- I probably should make newtypes for Electrons, Jets, etc.
instance FromJSON Event where
    parseJSON v = do els <- parseElectrons v
                     ljs <- parseLargeJets v
                     Event <$> parseBranch "runNumber" v
                           <*> parseBranch "eventNumber" v
                           <*> parseBranch "mu" v
                           <*> return els
                           <*> parseMuons v
                           <*> parseJets v
                           <*> return (V.filter (ljetSelection els) ljs)
                           <*> parseTrackJets v
                           <*> parseMET v
                           <*> ((/= 0) <$> (parseBranch "elelJ" v :: Parser Int))
                           <*> ((/= 0) <$> (parseBranch "mumuJ" v :: Parser Int))
                           <*> ((/= 0) <$> (parseBranch "elmuJ" v :: Parser Int))
                           <*> ((/= 0) <$> (parseBranch "elnuJ" v :: Parser Int))
                           <*> ((/= 0) <$> (parseBranch "munuJ" v :: Parser Int))
                           <*> ((/= 0) <$> (parseBranch "nunuJ" v :: Parser Int))



evtWeights :: [Text]
evtWeights = ["weight_mc", "weight_pileup", "weight_leptonSF"]

evtSystWeights :: [Text]
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
