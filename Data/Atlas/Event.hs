{-# LANGUAGE DeriveGeneric,OverloadedStrings #-}

module Data.Atlas.Event where

import Data.HEP.LorentzVector
import Data.Atlas.Electron
import Data.Atlas.Muon
import Data.Atlas.Jet

import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text, unpack)

import Data.Binary
import GHC.Generics (Generic)

import Control.Applicative

data Event = Event {
    eRunNumber :: Int,
    eEventNumber :: Int,
    eMCChannelNumber :: Int,
    eEventWeights :: Map Text Double,
    eWeightVariations :: Map Text Double,
    eMu :: Double,
    eElectrons :: Electrons,
    eMuons :: Muons,
    eJets :: Jets,
    eLargeJets :: LargeJets,
    eMET :: PtEtaPhiE
    } deriving (Show, Generic)

instance Binary Event

type Events = [Event]


data Cut a = CBool Text (a -> Bool)
           | CInt Text (a -> Int)

data CutResult = CRBool Text Bool
                 | CRInt Text Int
                deriving (Eq, Ord)

cName :: Cut a -> Text
cName (CBool t _) = t
cName (CInt t _) = t

crName :: CutResult -> Text
crName (CRBool t _) = t
crName (CRInt t _) = t

cApply :: Cut a -> a -> CutResult
cApply (CBool t f) x = CRBool t (f x)
cApply (CInt t f) x = CRInt t (f x)

instance Show (Cut a) where
    show = unpack . cName

instance Show CutResult where
    show (CRBool t x) = unpack t ++ show x
    show (CRInt t x) = unpack t ++ show x

type Region = [CutResult]

categorize :: [Cut a] -> a -> Region
categorize cuts x = fmap (`cApply` x) cuts

categorizes :: [Cut a] -> [a] -> Map Region [a]
categorizes cuts = foldr (uncurry (M.insertWith (++)) . (\x -> (categorize cuts x, [x]))) M.empty


nBtag :: Cut Event
nBtag = CInt "nBtag" $ length . filter ((> 0.4) <$> jMV2c20) . eJets

nJets :: Cut Event
nJets = CInt "nJets" $ length . filter ((> 25000) <$> (lvPt . jPtEtaPhiE)) . eJets

nLargeJets :: Cut Event
nLargeJets = CInt "nLargeJets" $ length . filter ((> 200000) <$> (lvPt . ljPtEtaPhiE)) . eLargeJets
