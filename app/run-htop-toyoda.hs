{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import           Atlas
import           Atlas.ToYoda
import           BFrag.Systematics
import           Control.Lens
import qualified Data.Text         as T
import qualified Data.Text.IO      as T


main :: IO ()
main = mainWith writeFiles

-- TODO
-- migration matrices
-- etc.

writeFiles :: Double -> String -> ProcMap (Folder (Vars YodaObj)) -> IO ()
writeFiles lu outf pm' = do
  let (pm, d) = collapseProcs pm'
      d' = over (_Just.traverse.noted) (scaleH $ 1.0/lu) d
      pm'' = variationsToMap "PowPyNom" (sequenceA pm) & at "data" .~ d'
      f varname hs =
        T.writeFile
          (outf ++ '/' : T.unpack varname ++ ".yoda")
          (ifoldMap printYodaObj $ folderToMap hs)

  imapM_ f pm''

collapseProcs
  :: ProcMap (Folder (Vars YodaObj))
  -> (Folder (Vars YodaObj), Maybe (Folder YodaObj))
collapseProcs pm =
  let preds = sans 0 pm
      dat = (fmap.fmap) (view nominal) $ pm ^. at 0
      preds' = ttbarSysts preds

  -- TODO
  -- partial
  in (preds' ^?! ix 410000, dat)
