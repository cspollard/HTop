{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE OverloadedStrings         #-}


module Data.Atlas.Systematics
  ( module X, evtWgt
  ) where

import           Data.Atlas.Corrected
import           Data.Atlas.Variation as X
import           Data.Monoid
import           Data.TTree
import           GHC.Float

evtWgt :: MonadIO m => Bool -> TR m (Vars SF)
evtWgt isData
  | isData = return mempty
  | otherwise = do
    pu <- puWgt
    evtw <-
      fmap (pure . sf "evtw" . float2Double . product) . traverse readBranch
        $ (["EvtW", "SFZVtx", "SFJVT"] :: [String])
    return $ evtw <> pu

puWgt :: MonadIO m => TR m (Vars SF)
puWgt = do
  puw <- float2Double <$> readBranch "SFPileUp"
  puwup <- float2Double <$> readBranch "SFPileUp_UP"
  puwdown <- float2Double <$> readBranch "SFPileUp_Down"
  return . fmap (sf "pileupwgt") . Variations puw
    $ [("puwup", puwup), ("puwdown", puwdown)]
