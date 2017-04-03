{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import           Atlas
import           Atlas.ToYoda
import           BFrag.Systematics
import           Control.Applicative
import           Control.Lens
import qualified Data.Histogram.Generic as H
import qualified Data.Map.Strict        as M
import           Data.Maybe             (fromJust)
import qualified Data.Text              as T
import qualified Data.Vector.Generic    as VG
import           Pipes                  ((<-<))
import qualified Pipes                  as P
import           Pipes.Prelude          as P
import           System.IO

main :: IO ()
main = mainWith writeFiles

-- TODO
-- migration matrices
-- etc.

writeFiles :: Double -> String -> ProcMap (Folder (Vars YodaObj)) -> IO ()
writeFiles _ outf pm' =
  let (pm, d) = over (_1.traverse) (liftA2 scaleH' lumi) $ collapseProcs pm'

      pm'' = variationsToMap "PowPyNom" (sequenceA pm) & at "data" .~ d

      -- TODO
      -- TODO
      -- THIS NAME CHANGED RECENTLY
      -- partial
      mats =
        pm''
        <&> filterFolder (Just "recozbtvstruezbt$")

      truths =
        pm''
        <&> set outOfRange Nothing
          . over histVals (view sumW)
          . (^?! ix "/elmujj/truthjets/zbt" . noted . _H1DD)
          . folderToMap

      -- TODO
      -- yoda is going to divide these by the (2D) bin width before drawing
      -- them; should we compensate?
      migs =
        M.intersectionWith
          (\t m -> over (noted._H2DD) (normToX t) <$> m)
          truths
          mats

      migs' = migs <&> inF (M.mapKeysMonotonic (`mappend` "mig"))

      pm''' = M.intersectionWith mappend pm'' migs'

      write varname hs =
        withFile (outf ++ '/' : T.unpack varname ++ ".yoda") WriteMode $ \h ->
          P.runEffect
          $ P.toHandle h
            <-< P.map (T.unpack . uncurry printYodaObj)
            <-< P.each (M.toList hs)

  in
    P.runEffect
    $ P.mapM_ (uncurry write) <-< P.each (M.toList $ folderToMap <$> pm''')


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


scaleH' :: Double -> Annotated Obj -> Annotated Obj
scaleH' x = over noted (scaleH x)

normAlongY :: (Bin bx, BinEq by) => Hist2D bx by -> Hist2D bx by
normAlongY = H.liftY (set integral 1)

normAlongX :: (Bin by, BinEq bx) => Hist2D bx by -> Hist2D bx by
normAlongX = H.liftX (set integral 1)

-- TODO
-- partial!
-- this is really some kind of traversal...
-- don't want to deal with uncertainty propagation yet...
normToX
  :: ( Fractional a, Weighted b, Weight b ~ a, VG.Vector v a, VG.Vector v b
     , BinEq bX, Bin bY )
  => Histogram v bX a
  -> Histogram v (Bin2D bX bY) b
  -> Histogram v (Bin2D bX bY) b
normToX h1 =
  H.liftX (fromJust . hzip (scaling . recip) h1)
