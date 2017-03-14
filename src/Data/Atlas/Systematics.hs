{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE OverloadedStrings         #-}


module Data.Atlas.Systematics
  ( module X, allVariations
  ) where

import           Data.Atlas.Corrected
import           Data.Atlas.Variation as X
import qualified Data.Map.Strict      as M
import qualified Data.Text            as T
import           Data.TTree
import           GHC.Float

type SystMap = M.Map T.Text

nomWeight :: MonadIO m => TR m Double
nomWeight =
  fmap (float2Double . product) . traverse readBranch
    $ (["EvtW", "SFPileUp", "SFZVtx", "SFJVT"] :: [String])

puWeightUp :: MonadIO m => TR m Double
puWeightUp = do
  w <- nomWeight
  puw <- float2Double <$> readBranch "SFPileUp"
  puwup <- float2Double <$> readBranch "SFPileUp_UP"
  return $ w * puwup / puw

puWeightDown :: MonadIO m => TR m Double
puWeightDown = do
  w <- nomWeight
  puw <- float2Double <$> readBranch "SFPileUp"
  puwdwn <- float2Double <$> readBranch "SFPileUp_DOWN"
  return $ w * puwdwn / puw

weightVars :: MonadIO m => TR m (SystMap SF)
weightVars = sequence . (fmap.fmap) (sf "evtw") $
  [ ("nominal", nomWeight)
  , ("pileupup", puWeightUp)
  , ("pileupdown", puWeightDown)
  ]

allVariations :: MonadIO m => [(String, TR m (SystMap SF))]
allVariations =
  [ ("nominal", weightVars)
  ]
