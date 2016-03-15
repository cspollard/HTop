{-# LANGUAGE OverloadedStrings #-}

module Data.HEP.Atlas.TopTree where

import Data.List (sortBy)
import Data.Ord (comparing, Down(..))
import qualified Data.Map as M
import Data.Maybe (fromJust)

import Control.Applicative
import Data.Vector (Vector(..))
import qualified Data.Vector as V

import Data.Text (Text, unpack)
import Data.Aeson (Value(..), withObject, (.:), FromJSON(..))
import Data.Aeson.Types (Parser)

import Data.Monoid ((<>))
import Control.Monad (forM)
import Data.Maybe (maybeToList)

import Data.HEP.LorentzVector
import Data.HEP.Atlas.Event
import Data.HEP.Atlas.Electron
import Data.HEP.Atlas.Muon
import Data.HEP.Atlas.Jet
import Data.HEP.Atlas.Tree
import Data.HEP.Atlas.Sample

import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Binary
import Data.Binary.Get
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 (toStrict)
import Control.Monad.Catch (MonadThrow(..))


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
                        parseBranch "ljet_tau32" val


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
instance FromJSON Event where
    parseJSON v = do
    
                    Event <$>
                        parseBranch "runNumber" v <*>
                        parseBranch "eventNumber" v <*>
                        parseBranch "mcChannelNumber" v <*>
                        parseBranchMap evtWeights v <*>
                        fmap (M.insert "nominal" 1.0) (parseBranchMap evtSystWeights v) <*>
                        parseBranch "mu" v <*>
                        fmap ptSort (parseElectrons v) <*>
                        fmap ptSort (parseMuons v) <*>
                        fmap ptSort (parseJets v) <*>
                        fmap ptSort (parseLargeJets v) <*>
                        fmap ptSort (parseTrackJets v) <*>
                        parseMET v

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



sampleInfo :: MonadThrow m => Consumer ByteString m SampleInfo
sampleInfo = tree =$= CL.fold (<>) mempty

conduitEncode :: (Monad m, Binary a) => Conduit a m ByteString
conduitEncode = CL.map (toStrict . encode)

conduitDecode :: (Monad m, Binary a) => Conduit ByteString m a
conduitDecode = do mbs <- await
                   case mbs of
                       Nothing -> return ()
                       -- some streams seem to return an empty
                       -- ByteString instead of Nothing?????
                       Just "" -> conduitDecode
                       Just bs -> go $ runGetIncremental get `pushChunk` bs

    where
        -- TODO
        -- fail
        go (Fail x _ z) = fail (show x ++ " " ++ z)
        go (Done rest _ x) = leftover rest >> yield x >> conduitDecode
        go (Partial f) = go =<< f <$> await
