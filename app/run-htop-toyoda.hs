{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import           Control.Arrow        (first)
import           Control.Lens
import           Data.Atlas.ToYoda
import           Data.Atlas.Variation
import           Data.Foldable        (fold)
import qualified Data.IntMap.Strict   as IM
import qualified Data.Map.Strict      as M
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import           Data.YODA.Obj


main :: IO ()
main = mainWith writeFiles

writeFiles :: String -> ProcMap (Folder (Vars YodaObj)) -> IO ()
writeFiles outf pm' = do
  let (pm, d) = collapseProcs pm'
      pm'' = variationsToMap "PowPyNom" (sequenceA pm) & at "data" .~ d
      f varname hs =
        T.writeFile
        (outf ++ '/' : T.unpack varname ++ ".yoda")
        (ifoldMap printYodaObj $ folderToMap hs)

  imapM_ f pm''


-- TODO
-- there are variations on the data (mu rescaling)
-- that need to be taken into account here.
collapseProcs
  :: ProcMap (Folder (Vars YodaObj))
  -> (Folder (Vars YodaObj), Maybe (Folder YodaObj))
collapseProcs pm =
  let preds = sans 0 pm

      dat :: Maybe (Folder YodaObj)
      dat = (fmap.fmap) (view nominal) $ pm ^. at 0

      systttbarDSIDs = (+410000) <$> [1, 2, 3, 4] :: [Int]
      (systttbar', systpreds) =
        IM.partitionWithKey (\k _ -> k `elem` systttbarDSIDs) preds

      -- the "nominal" variations from the ttbar systematic samples
      -- are actually variations
      systttbar = over (traverse.traverse) (view nominal) systttbar'

      -- TODO
      -- partial!
      ttbar' = systpreds ^?! ix 410000

      bkgs =  fold $ sans 410000 systpreds

      ttbar :: Folder (Vars YodaObj)
      ttbar =
        foldr (\(k, fs) fv ->
          inF2 (M.intersectionWith (\s v -> v & at k ?~ s)) fs fv) ttbar'
          $ fmap (first (procDict IM.!))
            . IM.toList
            $ systttbar

  in (mappend ttbar bkgs, dat)

procDict :: IM.IntMap T.Text
procDict =
  [ (410000, "PowPyNom")
  , (410001, "PowPyRadUp")
  , (410002, "PowPyRadDown")
  , (410003, "aMCHer")
  , (410004, "PowHer")
  ]
