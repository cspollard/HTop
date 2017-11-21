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
import           BFrag.Model
import           BFrag.Systematics
import           Control.Applicative
import           Control.Lens           hiding (each)
import           Data.Bifunctor
import qualified Data.Histogram.Generic as H
import           Data.Semigroup
import qualified Data.Text              as T
import           GHC.Exts               (IsList (..))
import           Pipes
import qualified Pipes.Prelude          as P
import           System.IO



main :: IO ()
main = mainWith writeFiles


writeFiles :: String -> ProcMap (Folder (Vars YodaObj)) -> IO ()
writeFiles outf pm = do
  let (data', pred', bkgs, ttpreds) = either error id . bfragModel $ fmap sequenceA <$> pm
      predhs = imap appLumi pred'
      bkghs = imap appLumi bkgs
      ttpredhs =
        ((fmap.fmap) (view nominal) . imap appLumi . (fmap.fmap) pure)
        <$> ttpreds

      -- don't scale truth histograms to lumi
      appLumi t yo =
        if T.isInfixOf "/elmujjtrue/" t || T.isInfixOf "/elmujjmatched/" t
          then yo
          else
            over ylabel (T.replace "\\sigma" "n" . T.replace "pb" "1")
            $ yo <&> liftA2 scaleH lumi

      write :: Bool -> T.Text -> Folder YodaObj -> IO ()
      write normed varname hs =
        withFile (outf ++ '/' : T.unpack varname ++ ".yoda") WriteMode $ \h ->
          runEffect
          $ each (toList $ _toMap hs)
            >-> P.map trim
            >-> P.mapFoldable (if normed then addNorm else pure)
            >-> P.map (T.unpack . uncurry printYodaObj . first ("/htop" <>))
            >-> P.toHandle h

      trim :: (T.Text, YodaObj) -> (T.Text, YodaObj)
      trim (t, yo) = (t, over noted trim' yo)
        where
          trim' (H1DD h) = H1DD $ obsTrimmers t h
          trim' (P1DD h) = P1DD $ obsTrimmers t h
          trim' (H2DD h) =
            let f = obsTrimmers t
            in H2DD . H.liftX f $ H.liftY f h

      addNorm :: (T.Text, YodaObj) -> [(T.Text, YodaObj)]
      addNorm (t, yo) =
        [ (t, yo)
        , ( t <> "norm"
          , yo
            & set (noted._H1DD.integral) 1
              . over ylabel g
          )
        ]

        where
          g yl =
            if T.isInfixOf "\\sigma" yl
              then "\\ensuremath{\\frac{1}{\\sigma}}" <> yl
              else "\\ensuremath{\\frac{1}{n}}" <> yl


      psmc :: VarMap (Folder YodaObj)
      psmc = variationToMap "nominal" . sequence $ sequence <$> predhs

      pstt :: VarMap (Folder YodaObj)
      pstt = fromList . fmap (first procToText) . toList $ ttpredhs

      psbkg :: VarMap (Folder YodaObj)
      psbkg = [("background", view nominal . sequence $ sequence <$> bkghs)]

      psdata :: VarMap (Folder YodaObj)
      psdata = [("data", data')]


  runEffect $ each (toList $ psmc <> psdata <> pstt) >-> P.mapM_ (uncurry $ write True)
  runEffect $ each (toList psbkg) >-> P.mapM_ (uncurry $ write False)


collapseProcs
  :: ProcMap (Folder (Vars YodaObj))
  -> (ProcMap (Folder (Vars YodaObj)), Maybe YodaFolder)
collapseProcs pm =
  let preds = sans datakey pm
      dat = (fmap.fmap) (view nominal) $ pm ^. at datakey

  in (preds, dat)
