{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BFrag.Model where

import           Atlas
import           BFrag.Systematics
import           Control.Applicative (liftA2)
import           Control.Arrow       ((***))
import           Control.Lens
import qualified Data.HashMap.Strict as HM
import           Data.List           (partition)
import qualified Data.Map            as M
import           Data.Maybe          (fromMaybe)
import           Data.Semigroup      ((<>))
import qualified Data.Text           as T
import           GHC.Exts            (toList)



bfragModel
  :: StrictMap ProcessInfo (Folder (Annotated (Vars Obj)))
  -> Either String (Folder (Annotated Obj), Folder (Annotated (Vars Obj)), Folder (Annotated (Vars Obj)), StrictMap ProcessInfo (Folder YodaObj))
bfragModel procs = do
  zjets <-
    getProcs procs zjetskeys
    & traverse.traverse.traverse %~ addVar "ZJetsNormUp" (scaleO 1.3)
  stop <-
    (fmap.fmap) (addVar "STopNormUp" (scaleO 1.3))
    <$> getProcs procs stopkeys


  let nonttpred =
        inF (M.filterWithKey (\k _ -> filtReco k))
        $ zjets `mappend` stop


      filtReco k =
        not . any ($ k)
        $ [ T.isInfixOf "/elmujjmatched/"
          , T.isInfixOf "/elmujjtrue/"
          ]


  nom <- mappend nonttpred <$> getProcs procs [nomkey]
  afii <-
    getProcs procs [afiikey] & (traverse.traverse.noted) %~ view nominal
  radup <-
    getProcs procs [radupkey] & (traverse.traverse.noted) %~ view nominal
  raddown <-
    getProcs procs [raddownkey] & (traverse.traverse.noted) %~ view nominal
  me <-
    getProcs procs [amcpy8key] & (traverse.traverse.noted) %~ view nominal
  ps <-
    getProcs procs [powh7key] & (traverse.traverse.noted) %~ view nominal
  sherpa <-
    mappend nonttpred
    <$> getProcs procs [sherpakey] & (traverse.traverse.noted) %~ view nominal


  let nomnom =
        nom & (traverse.noted) %~ view nominal

      inFA2 f = inF2 (M.intersectionWith $ liftA2 f)

      afiidiff = inFA2 corrDiffO nomnom afii
      raddiff = inFA2 (fmap (scaleO 0.5) . corrDiffO) radup raddown
      me' = mappend me afiidiff
      ps' = mappend ps afiidiff
      radup' = mappend nomnom raddiff
      fullpred =
        nom
        & inFA2 (\v n -> n & variations . at "MEUp" ?~ v) me'
        & inFA2 (\v n -> n & variations . at "RadUp" ?~ v) radup'
        & inFA2 (\v n -> n & variations . at "PSUp" ?~ v) ps'
        & traverse.traverse %~ collapseVars


      data' =
        either
          ((fmap.fmap) (scaleO $ view nominal lumi) . const nomnom)
          ((fmap.fmap) (view nominal))
          $ getProcs procs [datakey]


  return (data', fullpred, nonttpred
    , fmap ((fmap.fmap) (view nominal) . mappend nonttpred) . strictMap
      $ inSM (HM.filterWithKey $ \k _ -> k `elem` ttkeys) procs)

  where
    toEither s Nothing  = Left s
    toEither _ (Just x) = return x

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
    corrDiffO _ _ = error "diffing two incompatible objects"

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

    ttkeys =
      [ nomkey, afiikey, radupkey, raddownkey, amcpy8key
      , powh7key, sherpakey, powp6key
      ]

nomkey, afiikey, radupkey, raddownkey, amcpy8key
  , powh7key, sherpakey, powp6key, datakey
  :: ProcessInfo
nomkey = ProcessInfo 410501 FS
afiikey = ProcessInfo 410501 AFII
radupkey = ProcessInfo 410511 AFII
raddownkey = ProcessInfo 410512 AFII
amcpy8key = ProcessInfo 410225 AFII
powh7key = ProcessInfo 410525 AFII
sherpakey = ProcessInfo 410252 AFII
powp6key = ProcessInfo 410000 FS
datakey = ProcessInfo 0 DS

zjetskeys, stopkeys :: [ProcessInfo]
zjetskeys = flip ProcessInfo FS <$> [364128..364141]
stopkeys = flip ProcessInfo FS <$> [410015, 410016]


procToText :: ProcessInfo -> T.Text
procToText (ProcessInfo 410501 FS)   = "PowPy8FS"
procToText (ProcessInfo 410501 AFII) = "PowPy8AFII"
procToText (ProcessInfo 410511 AFII) = "PowPy8RadUpAFII"
procToText (ProcessInfo 410512 AFII) = "PowPy8RadDownAFII"
procToText (ProcessInfo 410225 AFII) = "aMCPy8AFII"
procToText (ProcessInfo 410525 AFII) = "PowH7AFII"
procToText (ProcessInfo 410252 AFII) = "Sherpa221AFII"
procToText (ProcessInfo 410000 FS)   = "PowPy6FS"
procToText (ProcessInfo 0 DS)        = "data"
procToText _                         = "OTHER"
