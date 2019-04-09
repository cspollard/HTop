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
import           Data.Vector            (Vector)
import           GHC.Exts               (IsList (..))
import           Pipes
import qualified Pipes.Prelude          as P
import           System.IO



main :: IO ()
main = mainWith writeFiles


writeFiles :: String -> ProcMap (Folder (Annotated (Vars Obj))) -> IO ()
writeFiles outf pm = do
  let (data', pred', bkgs, ttpreds) =
        either error id $ bfragModel Nothing pm

      predhs = imap appLumi pred'
      bkghs = imap appLumi bkgs
      ttpredhs =
        ((fmap.fmap) (view nominal) . imap appLumi . (fmap.fmap) pure)
        <$> ttpreds

      -- don't scale truth histograms to lumi
      appLumi t yo =
        if T.isInfixOf "/elmujjtrue/" t
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
      trim (t, yo) = (t, trim' <$> yo)
        where
          t' = T.takeWhileEnd (/= '/') t

          trim1D
            :: (Fractional a, Ord a, Monoid b)
            => Histogram Vector (ArbBin a) b -> Histogram Vector (ArbBin a) b
          trim1D =
            case (T.isInfixOf "probejets" t, T.isInfixOf "truejets" t) of
              (True, False) -> obsRecoTrimmers t'
              (False, True) -> obsTruthTrimmers t'
              _             -> id

          trim' (H1DD h) = H1DD $ trim1D h
          trim' (P1DD h) = P1DD $ trim1D h
          trim' h        = h
          -- trim' (H2DD h) =
          --   let fx = obsRecoTrimmers t'
          --   in H2DD . H.liftX f $ H.liftY f h

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
