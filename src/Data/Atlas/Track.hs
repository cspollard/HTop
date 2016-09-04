{-# LANGUAGE DeriveGeneric #-}

module Data.Atlas.Track where

import Data.TTree
import qualified Data.Vector as V

import GHC.Generics (Generic)
import Data.Serialize

import Data.HEP.LorentzVector


data Vertex = PVertex | SVertex deriving (Show, Ord, Eq, Generic)

class HasVertex hv where
    vertex :: hv -> Vertex

instance Serialize Vertex

data Track = Track { _tfourmom :: PtEtaPhiE
                   , _tvertex :: Vertex
                   } deriving (Show, Generic)

instance Serialize Track

instance HasVertex Track where
    vertex = _tvertex

instance HasLorentzVector Track where
    lv = fromLV . _tfourmom


jetTracksTLV :: MonadIO m
             => Vertex -> String -> String -> String -> String -> TR m [[Track]]
jetTracksTLV v spt seta sphi se = do trkpts <- fromVVector <$> readBranch spt
                                     trketas <- fromVVector <$> readBranch seta
                                     trkphis <- fromVVector <$> readBranch sphi
                                     trkes <- fromVVector <$> readBranch se

                                     let trks = V.zipWith4 (\pts etas phis es ->
                                                             V.toList $ V.zipWith4 PtEtaPhiE pts etas phis es
                                                            ) trkpts trketas trkphis trkes

                                     return $ fmap (flip Track v) <$> V.toList trks

trkEq :: Track -> Track -> Bool
Track p _ `trkEq` Track p' _ = (p `lvDR` p') < 0.005
