{-# LANGUAGE DeriveGeneric #-}

module Data.Atlas.Jet where

import Data.HEP.LorentzVector
import Data.Vector (Vector, (!))

import Data.Aeson (FromJSON(..))
import Control.Applicative ((<|>))

import Data.Serialize
import GHC.Generics (Generic)


data Jet = Jet {
    jPtEtaPhiE :: PtEtaPhiE,
    jMV2c20 :: Double,
    jJVT :: Double
    } deriving (Show, Generic)

instance Serialize Jet

instance HasLorentzVector Jet where
    lv = fromLV . jPtEtaPhiE

type Jets = Vector Jet


-- necessary for tauXY variables, which can be NaN
data SafeDouble = Doub { toDouble :: Double }
                | NaN
                deriving (Eq, Show, Read, Generic)


sdToMaybe :: SafeDouble -> Maybe Double
sdToMaybe (Doub d) = Just d
sdToMaybe _ = Nothing

instance Serialize SafeDouble where


instance FromJSON SafeDouble where
    parseJSON v = (Doub <$> parseJSON v) <|> (read <$> parseJSON v)


data LargeJet = LargeJet
              { ljPtEtaPhiE :: PtEtaPhiE
              , ljM :: Double
              , ljSD12 :: Double
              , ljTau21 :: SafeDouble
              , ljTau32 :: SafeDouble
              , ljGhostTJs :: [Int]
              } deriving (Show, Generic)

nGhostTags :: TrackJets -> LargeJet -> Int
nGhostTags tjs = length . filter bTagged . map (tjs !) . ljGhostTJs

instance Serialize LargeJet

instance HasLorentzVector LargeJet where
    lv = fromLV . ljPtEtaPhiE

type LargeJets = Vector LargeJet


data TrackJet = TrackJet {
    tjPtEtaPhiE :: PtEtaPhiE,
    tjMV2c20 :: Double,
    -- this won't be present in data.
    tjLabel :: Maybe Int
    } deriving (Show, Generic)


bTagged :: TrackJet -> Bool
bTagged = (> -0.3098) . tjMV2c20

instance Serialize TrackJet

instance HasLorentzVector TrackJet where
    lv = fromLV . tjPtEtaPhiE

type TrackJets = Vector TrackJet