{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeFamilies              #-}


module Data.Atlas.Systematics
  ( module X, evtWgt, ttbarSysts
  ) where

import           Control.Arrow        (first)
import           Control.Lens
import           Data.Atlas.Corrected
import           Data.Atlas.Variation as X
import qualified Data.IntMap          as IM
import qualified Data.Map             as M
import           Data.Monoid
import qualified Data.Text            as T
import           Data.TTree
import           Data.YODA.Obj
import           GHC.Float


type ProcMap = IM.IntMap

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
  puwdown <- float2Double <$> readBranch "SFPileUp_DOWN"
  return . fmap (sf "pileupwgt") . Variations puw
    $ [("puwup", puwup), ("puwdown", puwdown)]

ttbarSysts
  :: ProcMap (Folder (Vars YodaObj)) -> ProcMap (Folder (Vars YodaObj))
ttbarSysts preds =
  let systttbarDSIDs = (+410000) <$> [1, 2, 3, 4] :: [Int]
      (systttbar', systpreds) =
        IM.partitionWithKey (\k _ -> k `elem` systttbarDSIDs) preds

      -- the "nominal" variations from the ttbar systematic samples
      -- are actually variations
      systttbar = over (traverse.traverse) (view nominal) systttbar'

      addSysts x =
        foldr (\(k, fs) fv ->
          inF2 (M.intersectionWith (\s v -> v & at k ?~ s)) fs fv) x
          $ fmap (first (procDict IM.!))
            . IM.toList
            $ systttbar

  in over (ix 410000) addSysts systpreds


procDict :: IM.IntMap T.Text
procDict =
  [ (410000, "PowPyNom")
  , (410001, "PowPyRadUp")
  , (410002, "PowPyRadDown")
  , (410003, "aMCHer")
  , (410004, "PowHer")
  ]
