{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BFrag.Model where

import           Atlas
import           Atlas.CrossSections
import           BFrag.Systematics
import           Control.Applicative (liftA2)
import           Control.Arrow       ((***))
import           Control.Lens
import qualified Data.HashMap.Strict as HM
import           Data.List           (partition)
import qualified Data.Map            as M
import           Data.Maybe          (fromMaybe)
import           Data.Semigroup      (Sum (..), (<>))
import qualified Data.Text           as T
import           GHC.Exts            (toList)



bfragModel
  :: CrossSectionInfo
  -> StrictMap ProcessInfo (Sum Double, Folder (Vars YodaObj))
  -> Either String (Folder (Annotated Obj), Folder (Annotated (Vars Obj)))
bfragModel xsecs procs = do
  normedProcs <- itraverse normToXsec procs

  (zjets :: Folder (Annotated (Vars Obj))) <-
    getProcs normedProcs zjetskeys
    & traverse.traverse.traverse %~ addVar "ZJetsNormUp" (scaleO 1.3)
  stop <-
    (fmap.fmap) (addVar "STopNormUp" (scaleO 1.3))
    <$> getProcs normedProcs stopkeys


  let nonttbar =
        inF (M.filterWithKey (\k _ -> filtTrue k))
        $ zjets `mappend` stop

      filtTrue k =
        not . or $ ($ k) <$>
        [ T.isInfixOf "/elmujjmatched/"
        , T.isInfixOf "/elmujjtrue/"
        ]


  nom <- mappend nonttbar <$> getProcs normedProcs [nomkey]
  afii <-
    getProcs normedProcs [afiikey] & (traverse.traverse.noted) %~ view nominal
  radup <-
    getProcs normedProcs [radupkey] & (traverse.traverse.noted) %~ view nominal
  raddown <-
    getProcs normedProcs [raddownkey] & (traverse.traverse.noted) %~ view nominal
  me <-
    getProcs normedProcs [mekey] & (traverse.traverse.noted) %~ view nominal
  ps <-
    getProcs normedProcs [pskey] & (traverse.traverse.noted) %~ view nominal


  let nomnom = nom & (traverse.noted) %~ view nominal

      -- do an (a -> b -> c) inside a Folder (Annotated a/b)
      inFA2 f = inF2 (M.intersectionWith $ liftA2 f)

      afiidiff = inFA2 corrDiffO nomnom afii
      raddiff = inFA2 (fmap (scaleO 0.5) . corrDiffO) radup raddown
      me' = mappend me afiidiff
      ps' = mappend ps afiidiff
      radup' = mappend nomnom raddiff
      prediction =
        nom
        & inFA2 (\v n -> n & variations . at "MEUp" ?~ v) me'
        & inFA2 (\v n -> n & variations . at "RadUp" ?~ v) radup'
        & inFA2 (\v n -> n & variations . at "PSUp" ?~ v) ps'
        & traverse.traverse %~ collapseVars


      data' =
        either
          ((fmap.fmap) (scaleO $ view nominal lumi) . const nomnom)
          ((fmap.fmap) (view nominal))
          $ getProcs normedProcs [datakey]


  return (data', prediction)

  where
    toEither s Nothing  = Left s
    toEither _ (Just x) = return x

    normToXsec
      :: ProcessInfo
      -> (Sum Double, Folder (Vars YodaObj))
      -> Either String (Folder (Annotated (Vars Obj)))
    normToXsec (ProcessInfo _ DS) (_, hs) = return $ sequenceA <$> hs
    normToXsec (ProcessInfo ds _) (Sum w, hs) = do
      xsec <-
        toEither ("missing cross section for dsid " ++ show ds)
        $ xsecs ^? ix ds . _1
      return $ sequenceA . (fmap.fmap) (scaleO (xsec/w)) <$> hs

    scaleO :: Double -> Obj -> Obj
    scaleO w (H1DD h) = H1DD $ scaling w h
    scaleO w (H2DD h) = H2DD $ scaling w h
    scaleO w (P1DD h) = P1DD $ scaling w h

    corrDiffO :: Obj -> Obj -> Obj
    corrDiffO (H1DD h) (H1DD h') =
      fromMaybe (error "error diffing H1DDs") $ H1DD <$> removeSubHist h h'
    corrDiffO (H2DD h) (H2DD h') =
      fromMaybe (error "error diffing H2DDs") $ H2DD <$> removeSubHist h h'
    corrDiffO (P1DD h) (P1DD h') =
      fromMaybe (error "error diffing P1DDs") $ P1DD <$> removeSubHist h h'
    corrDiffO _ _ = error "diffing two u objects"

    nomkey = ProcessInfo 410501 FS
    afiikey = ProcessInfo 410501 AFII
    radupkey = ProcessInfo 410511 AFII
    raddownkey = ProcessInfo 410512 AFII
    mekey = ProcessInfo 410225 AFII
    pskey = ProcessInfo 410525 AFII
    datakey = ProcessInfo 0 DS

    zjetskeys = flip ProcessInfo FS <$> [364128..364141]
    stopkeys = flip ProcessInfo FS <$> [410015, 410016]

    addVar name f vs = vs & variations . at name ?~ f (view nominal vs)

    getProcs
      :: StrictMap ProcessInfo (Folder (Annotated (Vars Obj)))
      -> [ProcessInfo]
      -> Either String (Folder (Annotated (Vars Obj)))
    getProcs hs pis = do
      let getProc p = toEither ("missing " ++ show p) $ view (at p) hs
      hs' <- traverse getProc pis
      return $ mconcat hs'


    vardiff nom varup vardown =
      (<>) nom . scaleO 0.5 $ corrDiffO varup vardown

    collapseVars :: Vars Obj -> Vars Obj
    collapseVars (Variation n vs) =
      let vs' = toList vs
          filt s = T.isInfixOf (T.toLower s) . T.toLower
          rm s =
            fmap $ \(x, y) ->
              let x' = T.toLower x
                  s' = T.toLower s
                  x'' = mconcat $ T.splitOn s' x'
              in (x'', y)

          (downs, ups) =
            ((HM.fromList . rm "down") *** (HM.fromList . rm "up"))
            $ partition (filt "down" . fst) vs'
      in Variation n . strictMap $ HM.unionWith (vardiff n) ups downs
