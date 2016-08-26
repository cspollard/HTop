module Data.Atlas.LorentzVector ( lvsFromTTree ) where

import GHC.Float
import Control.Applicative (ZipList(..))
import Data.HEP.LorentzVector
import Data.TTree

newtype PtEtaPhiEs = PtEtaPhiEs [PtEtaPhiE]

lvsFromTTree :: MonadIO m => String -> TTreeRead m a
lvsFromTTree pre = PtEtaPhiEs . getZipList $ fmap PtEtaPhiE <$> (fmap.fmap) float2Double (readBranch (pre ++ "_pt"))
                                                            <*> (fmap.fmap) float2Double (readBranch (pre ++ "_eta"))
                                                            <*> (fmap.fmap) float2Double (readBranch (pre ++ "_phi"))
                                                            <*> (fmap.fmap) float2Double (readBranch (pre ++ "_e"))
