{-# LANGUAGE DeriveGeneric #-}

module Data.HEP.Atlas.Jet where

import Data.HEP.LorentzVector
import Data.Vector (Vector(..))
import qualified Data.Vector as V

import Data.Aeson (FromJSON(..))
import Control.Applicative ((<|>))

import Data.Binary
import GHC.Generics (Generic)

-- TODO
-- move out of this file
instance Binary a => Binary (Vector a) where
    put v = put (V.length v :: Int) >> V.mapM_ put v
    get = (get :: Get Int) >>= flip V.replicateM get


data Jet = Jet {
    jPtEtaPhiE :: PtEtaPhiE,
    jMV2c20 :: Double,
    jJVT :: Double
    } deriving (Show, Generic)

instance Binary Jet

instance HasLorentzVector Jet where
    lv = fromLV . jPtEtaPhiE

type Jets = Vector Jet


data SafeDouble = Doub { toDouble :: Double }
                  | NaN
                  deriving (Eq, Show, Read, Generic)

instance Binary SafeDouble where


instance FromJSON SafeDouble where
    parseJSON v = (Doub <$> parseJSON v) <|> (read <$> parseJSON v)


data LargeJet = LargeJet {
    ljPtEtaPhiE :: PtEtaPhiE,
    ljM :: Double,
    ljSD12 :: Double,
    ljTau21 :: SafeDouble,
    ljTau32 :: SafeDouble,
    ljTJets :: Vector Int
    } deriving (Show, Generic)

instance Binary LargeJet

instance HasLorentzVector LargeJet where
    lv = fromLV . ljPtEtaPhiE

type LargeJets = Vector LargeJet

data TrackJet = TrackJet {
    tjPtEtaPhiE :: PtEtaPhiE,
    tjMV2c20 :: Double
    } deriving (Show, Generic)

instance Binary TrackJet

instance HasLorentzVector TrackJet where
    lv = fromLV . tjPtEtaPhiE

type TrackJets = Vector TrackJet
