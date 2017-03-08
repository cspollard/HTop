{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import           Control.Arrow      (first)
import           Control.Lens
import           Data.Atlas.ToYoda
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict    as M
import qualified Data.Text          as T
import qualified Data.Text.IO       as T
import           Data.YODA.Obj


-- TODO
-- this does not deal with systematic uncertainties at all.

main :: IO ()
main = mainWith writeFiles

writeFiles :: String -> ProcMap (SystMap YodaFolder) -> IO ()
writeFiles outf pm =
  iforM_ (collapseProcs pm) $
      \ds hs -> T.writeFile (outf ++ '/' : T.unpack ds ++ ".yoda")
                  (ifoldMap printYodaObj hs)

collapseProcs :: ProcMap (SystMap YodaFolder) -> SystMap YodaFolder
collapseProcs pm =
  let preds = sans 0 pm

      dat = pm ^. at 0 . _Just . at "data"

      systttbarDSIDs = (+410000) <$> [1, 2, 3, 4] :: [Int]
      (systttbar', systpreds) =
        IM.partitionWithKey (\k _ -> k `elem` systttbarDSIDs) preds
      systttbar = (M.! "nominal") <$> systttbar'

      nombkgs = sans 410000 systpreds <&> (M.! "nominal")
      f x = foldr mergeYF x nombkgs
      systttbartots = f <$> systttbar
      modelsysts =
        M.fromList
        . fmap (first (procDict IM.!))
        . IM.toList
        $ systttbartots

      systs = foldr1 (M.unionWith mergeYF) systpreds `M.union` modelsysts
  in set (at "data") dat systs

procDict :: IM.IntMap T.Text
procDict =
  [ (410000, "PowPyNom")
  , (410001, "PowPyRadUp")
  , (410002, "PowPyRadDown")
  , (410003, "aMCHer")
  , (410004, "PowHer")
  ]
