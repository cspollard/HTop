{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import           Atlas
import           Atlas.ToYoda
import           BFrag.BFrag
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


writeFiles :: String -> ProcMap (Folder (Vars YodaObj)) -> IO ()
writeFiles outf pm' = do
  let datakey = ProcessInfo 0 DS
      datahs = (fmap.fmap) (view nominal) $ pm' ^? ix datakey
      mchs' = sans datakey pm'
      mchs = imap appLumi <$> mchs'

      appLumi t yo =
        if T.isInfixOf "elmujjtrue" t
          then yo
          else liftA2 scaleH' lumi yo

      write :: T.Text -> M.Map T.Text YodaObj -> IO ()
      write varname hs =
        withFile (outf ++ '/' : T.unpack varname ++ ".yoda") WriteMode $ \h ->
          runEffect
          $ each (M.toList hs)
            >-> P.map trim
            >-> P.map (T.unpack . uncurry printYodaObj . first ("/htop" <>))
            >-> P.toHandle h

      trim :: (T.Text, YodaObj) -> (T.Text, YodaObj)
      trim (t, yo)
        | t == truehname = (t, over (noted._H1DD) trimTrueH yo)
        | t == recohname = (t, over (noted._H1DD) trimRecoH yo)
        | t == recomatchhname = (t, over (noted._H1DD) trimRecoH yo)
        | t == matrixname = (t, over (noted._H2DD) (H.liftX trimTrueH . H.liftY trimRecoH) yo)
        | otherwise = (t, yo)

      psmc :: [(T.Text, M.Map T.Text YodaObj)]
      psmc = toList . variationToMap "nominal" . sequence . _toMap $ view traverse mchs

      psdata :: [(T.Text, M.Map T.Text YodaObj)]
      psdata =
        case datahs of
          Just hs -> [("data", _toMap hs)]
          Nothing -> []

  runEffect $ each (psmc ++ psdata) >-> P.mapM_ (uncurry write)


-- TODO
-- we only grab the first dsid currently
-- we are ignoring modeling variations here
-- as well as backgrounds
-- partial
collapseProcs
  :: ProcMap (Folder (Vars YodaObj))
  -> (ProcMap (Folder (Vars YodaObj)), Maybe YodaFolder)
collapseProcs pm =
  let datakey = ProcessInfo 0 DS
      preds = sans datakey pm
      dat = (fmap.fmap) (view nominal) $ pm ^. at datakey

  -- TODO
  -- partial
  in (preds, dat)


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
