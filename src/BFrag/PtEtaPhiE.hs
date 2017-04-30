module BFrag.PtEtaPhiE
  ( lvsFromTTreeF
  ) where

import           Control.Applicative    (ZipList (..))
import           Data.HEP.LorentzVector
import           Data.TTree
import           GHC.Float

lvsFromTTreeF
  :: (MonadIO m, MonadFail m)
  => String
  -> String
  -> String
  -> String
  -> TreeRead m (ZipList PtEtaPhiE)
lvsFromTTreeF ptn etan phin en = do
  pts <- fmap float2Double <$> readBranch ptn
  etas <- fmap float2Double <$> readBranch etan
  phis <- fmap float2Double <$> readBranch phin
  es <- fmap float2Double <$> readBranch en

  return $ PtEtaPhiE <$> pts <*> etas <*> phis <*> es
