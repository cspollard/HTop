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
import Data.Maybe (fromMaybe)
import Linear hiding (trace)
import qualified Data.Map as M
import qualified Data.Histogram.Generic as H
import           Data.Semigroup
import qualified Data.Text              as T
import           Data.Vector            (Vector)
import qualified Data.Vector as V
import           GHC.Exts               (IsList (..))
import           Pipes
import qualified Pipes.Prelude          as P
import           System.IO
import Control.Monad (foldM)
import Numeric (showFFloat)
import Debug.Trace
import qualified Data.Matrix as Mat


fromJust' :: String -> Maybe a -> a
fromJust' s = fromMaybe (error s)


-- only eta plots fail as far as I can tell.
dataChi2 :: Obj -> Vars Obj -> Maybe (Int, Double)
dataChi2 (P1DD _) _ = Nothing
dataChi2 (H2DD _) _ = Nothing
dataChi2 d vs = do
  if det > 0
      then Just (length d', chi2)
      else Nothing

  where
    vs' :: Vars (Vector (Dist1D Double))
    vs' = (^?! (_H1DD.histData)) <$> vs

    d', datastat :: Vector Double
    d' = view sumW <$> (d ^?! _H1DD . histData)
    datastat = view sumWW <$> (d ^?! _H1DD . histData)

    n', mcstat :: Vector Double
    n' = view sumW <$> view nominal vs'
    mcstat = view sumWW <$> view nominal vs'

    vars = fmap (view sumW) <$> vs'

    cov = binnedCovariance vars !+! scaled datastat !+! scaled mcstat

    mat = Mat.fromLists . fmap toList $ toList cov

    -- could use SVD but for now just throw out singular matrices
    det = Mat.detLU mat

    invcovmat = either (error . ("invcov: " ++)) id . Mat.inverse $ mat

    invcov = fromList . fmap fromList $ Mat.toLists invcovmat

    chi2 = let x = d' ^-^ n' in dot x $ x *! invcov


binnedCovariance :: (Num a, Functor f, Foldable f, Additive v) => Variation f (v a) -> v (v a)
binnedCovariance (Variation n vs) = foldl (!+!) (const zeroV <$> n) $ matdiff <$> vs
  where
    matdiff v = let v' = v ^-^ n in outer (*) v' v'

    outer :: (Functor w, Functor v) => (a -> a -> a) -> v a -> w a -> v (w a)
    outer f a b = (\x -> f x <$> b) <$> a

    zeroV = const 0 <$> n



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
  

pruneData :: Folder a -> Folder a
pruneData =
  inF . M.filterWithKey
  $ \k _ -> not $ "elmujjmatched" `T.isInfixOf` k || "elmujjtrue" `T.isInfixOf` k


main :: IO ()
main = mainWith writeFiles


writeFiles :: String -> ProcMap (Folder (Annotated (Vars Obj))) -> IO ()
writeFiles outf pm = do
  let (data', pred', bkgs, ttpreds) = either error id $ bfragModel Nothing pm

      predhs = imap appLumi pred'
      bkghs = imap (\k x -> set title "background" $ appLumi k x) bkgs

      ttpredhs :: StrictMap ProcessInfo (Folder (Annotated Obj))
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


      data'' = (fmap.fmap) (view nominal) . addNorm . (fmap.fmap) pure $ pruneData data'

      predhs' = addNorm predhs

      toths :: Folder YodaObj
      toths = over (traverse.traverse) totalUncert $ addChi2 data'' predhs'

      psmc :: VarMap (Folder YodaObj)
      psmc = variationToMap "nominal" . sequence $ sequence <$> predhs'


      pstt :: VarMap (Folder YodaObj)
      pstt =
        fromList
        . fmap (first procToText)
        . toList
        . (fmap.fmap.fmap) (view nominal)
        . fmap addNorm
        . (fmap.fmap.fmap) pure
        $ ttpredhs

      -- psbkg :: VarMap (Folder YodaObj)
      psbkg = ("background", view nominal . sequence $ sequence <$> bkghs)
      pstot = ("total", toths)

      -- psdata :: VarMap (Folder YodaObj)
      psdata = ("data", data'')


      -- I'm not sure why e.g. matched and truth histograms are getting a
      -- chi2...
      addChi2 :: Folder YodaObj -> Folder (Annotated (Vars Obj)) -> Folder (Annotated (Vars Obj))
      addChi2 = inF2 (M.mergeWithKey (\_k x y -> Just $ go x y) (const mempty) id)
        where
          go (Annotated _ dh) p@(Annotated _ ph) = maybe p id $ do
            (ndf, chi2) <- dataChi2 dh ph
            let chi2txt = T.pack $ showFFloat (Just 3) chi2 ""
                str = " (\\ensuremath{\\chi^2} / n.d.f = " <> chi2txt <> " / " <> T.pack (show ndf) <> ")"
            return $ set title ("prediction" <> str) p


  runEffect $ each (toList $ psmc <> pstt) >-> P.mapM_ (uncurry write)
  runEffect $ yield psdata >-> P.mapM_ (uncurry write)
  runEffect $ yield psbkg >-> P.mapM_ (uncurry write)
  runEffect $ yield pstot >-> P.mapM_ (uncurry write)


collapseProcs
  :: ProcMap (Folder (Vars YodaObj))
  -> (ProcMap (Folder (Vars YodaObj)), Maybe YodaFolder)
collapseProcs pm =
  let preds = sans datakey pm
      dat = (fmap.fmap) (view nominal) $ pm ^. at datakey

  in (preds, dat)


