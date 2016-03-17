{-# LANGUAGE DeriveGeneric #-}

module Data.HEP.Atlas.Jet where

import Data.HEP.LorentzVector
import Data.Vector (Vector(..))
import qualified Data.Vector as V

import Data.Aeson (FromJSON(..))
import Control.Applicative ((<|>))

import Data.Serialize
import Data.Serialize.Vector
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


instance Serialize SafeDouble where


instance FromJSON SafeDouble where
    parseJSON v = (Doub <$> parseJSON v) <|> (read <$> parseJSON v)


data LargeJet = LargeJet {
    ljPtEtaPhiE :: PtEtaPhiE,
    ljM :: Double,
    ljSD12 :: Double,
    ljTau21 :: SafeDouble,
    ljTau32 :: SafeDouble
    } deriving (Show, Generic)

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

instance Serialize TrackJet

instance HasLorentzVector TrackJet where
    lv = fromLV . tjPtEtaPhiE

type TrackJets = Vector TrackJet
