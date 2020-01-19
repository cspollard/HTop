{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE FlexibleContexts     #-}
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
import Control.Monad (foldM)


main :: IO ()
main = mainWith writeFiles


totalUncert :: Vars Obj -> Obj
totalUncert (Variation h@(H2DD _) _) = h
totalUncert (Variation h@(P1DD _) _) = h
totalUncert (Variation (H1DD n') vs') =
  let vs'' = maybe (error "not all H1DDs") id $ traverse (preview _H1DD) vs'
  in H1DD $ go n' vs''

  where
    go n vs =
      maybe (error "failed to calculate total uncert") id $ do
        let addSyst dn dsyst =
              let diff = view sumW dn - view sumW dsyst
              in over sumWW (+ diff*diff) dn

        foldM (hzip' addSyst) n vs
  

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

      write :: T.Text -> Folder YodaObj -> IO ()
      write varname hs =
        withFile (outf ++ '/' : T.unpack varname ++ ".yoda") WriteMode $ \h ->
          runEffect
          $ each (toList $ _toMap hs)
            >-> P.map trim
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


      addNorm :: Folder (Annotated (Vars Obj)) -> Folder (Annotated (Vars Obj))
      addNorm f = ifoldl go f f
        where
          go :: T.Text -> Folder (Annotated (Vars Obj)) -> Annotated (Vars Obj) -> Folder (Annotated (Vars Obj))
          go t f' avo = f' & at (t <> "norm") ?~ normalize avo


      normalize :: Annotated (Vars Obj) -> Annotated (Vars Obj)
      normalize =
        set (noted.nominal._H1DD.integral) 1
        . set (noted.variations.traverse._H1DD.integral) 1
        . over ylabel g
        where
          g yl =
            if T.isInfixOf "\\sigma" yl
              then "\\ensuremath{\\frac{1}{\\sigma}}" <> yl
              else "\\ensuremath{\\frac{1}{n}}" <> yl


      addTot v = let tot = totalUncert v in v & variations . at "total" ?~ tot

      predhs' = over (traverse.traverse) addTot $ addNorm predhs

      -- ttpreds' = over (traverse.traverse) addTot $ addNorm ttpreds

      psmc :: VarMap (Folder YodaObj)
      psmc = variationToMap "nominal" . sequence $ sequence <$> predhs'

      pstt :: VarMap (Folder YodaObj)
      pstt = fromList . fmap (first procToText) . toList $ ttpredhs

      psbkg :: VarMap (Folder YodaObj)
      psbkg = [("background", view nominal . sequence $ sequence <$> bkghs)]

      psdata :: VarMap (Folder YodaObj)
      psdata = [("data", (fmap.fmap) (view nominal) . addNorm $ (fmap.fmap) pure data')]


  runEffect $ each (toList $ psmc <> psdata <> pstt) >-> P.mapM_ (uncurry write)
  runEffect $ each (toList psbkg) >-> P.mapM_ (uncurry write)


collapseProcs
  :: ProcMap (Folder (Vars YodaObj))
  -> (ProcMap (Folder (Vars YodaObj)), Maybe YodaFolder)
collapseProcs pm =
  let preds = sans datakey pm
      dat = (fmap.fmap) (view nominal) $ pm ^. at datakey

  in (preds, dat)
