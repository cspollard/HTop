module Data.Atlas.LorentzVector ( lvFromTTree ) where

import Data.HEP.LorentzVector
import Data.TTree

newtype LorentzVectors = LorentzVectors [LorentzVector]

lvsFromTTree :: MonadIO m => String -> TTreeRead m a
lvsFromTTree pre = fromZipList . fmap PtEtaPhiE
                   <$> fmap float2Double (readBranch (pre ++ "_pt"))
                   <*> fmap float2Double (readBranch (pre ++ "_eta"))
                   <*> fmap float2Double (readBranch (pre ++ "_phi"))
                   <*> fmap float2Double (readBranch (pre ++ "_e"))
