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
import           Control.Lens           hiding (each)
import           Data.Bifunctor
import qualified Data.Histogram.Generic as H
import qualified Data.Map.Strict        as M
import           Data.Maybe             (fromJust)
import           Data.Semigroup
import qualified Data.Text              as T
import qualified Data.Vector.Generic    as VG
import           GHC.Exts               (IsList (..))
import           Pipes
import qualified Pipes.Prelude          as P
import           System.IO



main :: IO ()
main = mainWith writeFiles


-- TODO
-- we're not writing data.
-- partial
writeFiles :: String -> ProcMap (Folder (Vars YodaObj)) -> IO ()
writeFiles outf pm' = void . sequence $ do
  pm'' <- fst $ collapseProcs pm'
  let pm = liftA2 scaleH' lumi <$> pm''

      write :: T.Text -> M.Map T.Text YodaObj -> IO ()
      write varname hs =
        withFile (outf ++ '/' : T.unpack varname ++ ".yoda") WriteMode $ \h ->
          runEffect
          $ each (M.toList hs)
            >-> P.map (T.unpack . uncurry printYodaObj . first ("/htop" <>))
            >-> P.toHandle h

      ps :: [(T.Text, M.Map T.Text YodaObj)]
      ps = toList . variationToMap "nominal" . sequence $ folderToMap pm

  return . runEffect $ each ps >-> P.mapM_ (uncurry write)


-- TODO
-- we only grab the first dsid currently.
-- we are ignoring modeling variations here
-- as well as backgrounds
-- partial
collapseProcs
  :: ProcMap (Folder (Vars YodaObj))
  -> (Maybe (Folder (Vars YodaObj)), Maybe YodaFolder)
collapseProcs pm =
  let preds = sans 0 pm
      dat = (fmap.fmap) (view nominal) $ pm ^. at 0

  -- TODO
  -- partial
  in (firstOf traverse preds, dat)


scaleH' :: Double -> YodaObj -> YodaObj
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
