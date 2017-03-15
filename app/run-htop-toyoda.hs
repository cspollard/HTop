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


-- TODO
-- this does not deal with systematic uncertainties at all.

main :: IO ()
main = mainWith writeFiles

writeFiles :: String -> ProcMap (Vars (Folder YodaObj)) -> IO ()
writeFiles outf pm =
  iforM_ (variationsToMap "PowPyNom" $ collapseProcs pm) $
      \varname hs ->
        T.writeFile
        (outf ++ '/' : T.unpack varname ++ ".yoda")
        (ifoldMap printYodaObj $ folderToMap hs)

-- TODO
-- this will make n copies of the data histogram
-- because it will have no systematics.
-- laziness might prevent this, but still...
collapseProcs :: ProcMap (Vars (Folder YodaObj)) -> Vars (Folder YodaObj)
collapseProcs pm =
  let preds = sans 0 pm

      dat = pm ^. at 0 . _Just . at "data"

      systttbarDSIDs = (+410000) <$> [1, 2, 3, 4] :: [Int]
      (systttbar', systpreds) =
        IM.partitionWithKey (\k _ -> k `elem` systttbarDSIDs) preds

      -- the "nominal" variations from the ttbar systematic samples
      -- are actually variations
      systttbar = view nominal <$> systttbar'

      -- TODO
      -- partial!
      ttbar' = systpreds ^?! ix 410000

      bkgs = fold $ sans 410000 systpreds

      ttbar =
        foldr (\(k, hs) v -> v & at k ?~ hs) ttbar'
        $ fmap (first (procDict IM.!))
          . IM.toList
          $ systttbar

  in mappend ttbar bkgs

procDict :: IM.IntMap T.Text
procDict =
  [ (410000, "PowPyNom")
  , (410001, "PowPyRadUp")
  , (410002, "PowPyRadDown")
  , (410003, "aMCHer")
  , (410004, "PowHer")
  ]
