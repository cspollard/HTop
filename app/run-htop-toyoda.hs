{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import           Control.Lens
import           Data.Atlas.Systematics
import           Data.Atlas.ToYoda
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import           Data.YODA.Obj


main :: IO ()
main = mainWith writeFiles

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
  where
    scaleH :: Double -> Obj -> Obj
    scaleH x (H1DD h) = H1DD $ scaling x h
    scaleH x (P1DD h) = P1DD $ scaling x h
    scaleH x (H2DD h) = H2DD $ scaling x h

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
