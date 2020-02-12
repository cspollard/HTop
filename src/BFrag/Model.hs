{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BFrag.Model where

import           Atlas
import           BFrag.Systematics
import           Control.Applicative (liftA2)
import           Control.Arrow       ((***))
import           Control.Lens
import           Data.Foldable       (fold)
import qualified Data.HashMap.Strict as HM
import           Data.List           (partition)
import qualified Data.Map            as M
import           Data.Maybe          (fromMaybe)
import           Data.Semigroup      ((<>))
import qualified Data.Text           as T
import           GHC.Exts            (toList)



bfragModel
  :: Maybe String
  -> StrictMap ProcessInfo (Folder (Annotated (Vars Obj)))
  -> Either String
      ( Folder (Annotated Obj)
      , Folder (Annotated (Vars Obj))
      , Folder (Annotated (Vars Obj))
      , StrictMap ProcessInfo (Folder YodaObj)
      )

bfragModel stresstest procs = do
  zjets <-
    getProcs procs zjetskeys
    & traverse.traverse.traverse %~ addVar "ZJetsNormUp" (scaleO 1.3)

  stopdr <- getProcs procs stopdrkeys
  stopds <- getProcs procs stopdskeys & (traverse.traverse.noted) %~ view nominal

  let inFA2 f = inF2 (M.intersectionWith $ liftA2 f)
      stop =
        stopdr
        & inFA2 (\v n -> n & variations . at "STopDS" ?~ v) stopds
        & (fmap.fmap) (addVar "STopNormUp" (scaleO 1.3))

  diboson <-
    (fmap.fmap) (addVar "DibosonNormUp" (scaleO 1.3))
    <$> getProcs procs dibosonkeys


  let nonttpred =
        inF (M.filterWithKey (\k _ -> filtReco k))
        $ fold [zjets, stop, diboson]


      filtReco k =
        not . any ($ k)
        $ [ T.isInfixOf "/elmujjmatched/"
          , T.isInfixOf "/elmujjtrue/"
          ]

  nom <-
    mappend nonttpred
    . (fmap.fmap) (addVar "TTBarNormUp" (scaleO 1.20))
    <$> getProcs procs [nomkey]

  afii <-
    getProcs procs [afiikey] & (traverse.traverse.noted) %~ view nominal

  radup <-
    getProcs procs [radupkey] & (traverse.traverse.noted) %~ view nominal

  raddown <-
    getProcs procs [raddownkey] & (traverse.traverse.noted) %~ view nominal

  fsrup <-
    getProcs procs [fsrupkey] & (traverse.traverse.noted) %~ view nominal

  fsrdown <-
    getProcs procs [fsrdownkey] & (traverse.traverse.noted) %~ view nominal

  -- me <-
  --   getProcs procs [amcpy8key] & (traverse.traverse.noted) %~ view nominal

  ps <-
    getProcs procs [powh7key] & (traverse.traverse.noted) %~ view nominal

  sherpa <-
    mappend nonttpred
    <$> getProcs procs [sherpakey] & (traverse.traverse.noted) %~ view nominal


  let nomnom = nom & (traverse.noted) %~ view nominal

      afiidiff = inFA2 corrDiffO nomnom afii
      raddiff = inFA2 (fmap (scaleO 0.5) . corrDiffO) radup raddown
      fsrdiff = inFA2 (fmap (scaleO 0.5) . corrDiffO) fsrup fsrdown
      -- me' = mappend me afiidiff
      ps' = mappend ps afiidiff
      rad' = mappend nomnom raddiff
      fsr' = mappend nomnom fsrdiff

      fullpred =
        nom
        & inFA2 (\v n -> n & variations . at "RadUp" ?~ v) rad'
        & inFA2 (\v n -> n & variations . at "FSRUp" ?~ v) fsr'
        & inFA2 (\v n -> n & variations . at "PSUp" ?~ v) ps'
        & traverse.traverse %~ collapseVars


      data' :: Folder (Annotated Obj) =
        case stresstest of
          Nothing ->
            either error ((fmap.fmap) (view nominal)) $ getProcs procs [datakey]
          Just "closure" -> (fmap.fmap) (scaleO $ view nominal lumi) nomnom
          Just "mugt22" ->
            (fmap.fmap) (scaleO $ view nominal lumi)
            $ imap
                (\k v -> if T.take 7 k == "elmujj/" then nomnom ^?! ix (T.replace "elmujj/" "mu_gt_22/elmujj/" k) else v)
                nomnom

          Just "mult22" ->
            (fmap.fmap) (scaleO $ view nominal lumi)
            $ imap
                (\k v -> if T.take 7 k == "elmujj/" then nomnom ^?! ix (T.replace "elmujj/" "mu_lt_22/elmujj/" k) else v)
                nomnom

          Just var ->
            let vs = sequence $ sequence <$> fullpred :: Vars (Folder (Annotated Obj))
                mv = view (variations . at (T.pack var)) vs :: Maybe (Folder (Annotated Obj))
            in case mv of
              Nothing ->
                let ks = views variations (inSM HM.keys) vs
                in error $ "missing stress test variation " ++ var ++ ".\nall variations:\n" ++ show ks ++ "\n"

              Just fo -> (fmap.fmap) (scaleO $ view nominal lumi) fo


      ttprocs = inSM (HM.filterWithKey $ \k _ -> k `elem` ttkeys) procs

      ttpred =
        (fmap.fmap) (view nominal) . mappend nonttpred
        <$> strictMap ttprocs


  data'
    `seq` fullpred
    `seq` nonttpred
    `seq` ttpred
    `seq` return (data', fullpred, nonttpred, ttpred)

  where
    toEither s Nothing  = Left s
    toEither _ (Just x) = return x

    scaleO :: Double -> Obj -> Obj
    scaleO w (H1DD h) = H1DD $ scaling w h
    scaleO w (H2DD h) = H2DD $ scaling w h
    scaleO w (P1DD h) = P1DD $ scaling w h

    corrDiffO :: Obj -> Obj -> Obj
    corrDiffO (H1DD h) (H1DD h') =
      maybe (error "error diffing H1DDs") H1DD $ removeSubHist h h'
    corrDiffO (H2DD h) (H2DD h') =
      maybe (error "error diffing H2DDs") H2DD $ removeSubHist h h'
    corrDiffO (P1DD h) (P1DD h') =
      maybe (error "error diffing P1DDs") P1DD $ removeSubHist h h'
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

          rmEnd s =
            fmap $ \(x, y) ->
              let x' = T.toLower x
                  s' = T.toLower s
                  x'' = fromMaybe x' $ T.stripSuffix s' x'
              in (x'', y)

          (downs, ups) =
            ((HM.fromList . rmEnd "down") *** (HM.fromList . rmEnd "up"))
            $ partition (filt "down" . fst) vs'

      in Variation n . strictMap
          -- remove systs from LOOSE tracks.
          . HM.filterWithKey (\k _ -> not $ "loose" `T.isInfixOf` k || "npvtrksf" `T.isInfixOf` k)
          $ HM.unionWith (vardiff n) ups downs

    ttkeys =
      [ nomkey, afiikey, radupkey, raddownkey, fsrupkey, fsrdownkey, amcpy8key
      , powh7key, sherpakey, powp6key
      ]


nomkey, afiikey, radupkey, raddownkey, fsrupkey, fsrdownkey, amcpy8key
  , powh7key, sherpakey, powp6key, datakey
  :: ProcessInfo
nomkey = ProcessInfo 410501 FS
afiikey = ProcessInfo 410501 AFII
fsrupkey = ProcessInfo 410028 AFII
fsrdownkey = ProcessInfo 410029 AFII
radupkey = ProcessInfo 410511 AFII
raddownkey = ProcessInfo 410512 AFII
amcpy8key = ProcessInfo 410225 AFII
powh7key = ProcessInfo 410525 AFII
sherpakey = ProcessInfo 410252 AFII
powp6key = ProcessInfo 410000 FS
datakey = ProcessInfo 0 DS


zjetskeys, stopdrkeys, stopdskeys, dibosonkeys :: [ProcessInfo]
zjetskeys = flip ProcessInfo FS <$> [364128..364141]
stopdrkeys = flip ProcessInfo FS <$> [410015, 410016]
stopdskeys = flip ProcessInfo FS <$> [410064, 410065]
dibosonkeys = flip ProcessInfo FS <$> [361068, 361096]


procToText :: ProcessInfo -> T.Text
procToText (ProcessInfo 410501 FS)   = "PowPy8FS"
procToText (ProcessInfo 410501 AFII) = "PowPy8AFII"
procToText (ProcessInfo 410511 AFII) = "PowPy8RadUpAFII"
procToText (ProcessInfo 410512 AFII) = "PowPy8RadDownAFII"
procToText (ProcessInfo 410225 AFII) = "aMCPy8AFII"
procToText (ProcessInfo 410525 AFII) = "PowH7AFII"
procToText (ProcessInfo 410252 AFII) = "Sherpa221AFII"
procToText (ProcessInfo 410000 FS)   = "PowPy6FS"
procToText (ProcessInfo 410028 AFII) = "PowPy8FSRUpAFII"
procToText (ProcessInfo 410029 AFII) = "PowPy8FSRDownAFII"
procToText (ProcessInfo 0 DS)        = "data"
procToText _                         = "OTHER"


paramToName :: T.Text -> T.Text
paramToName s =
  if "normtruthbin" `T.isPrefixOf` s
    then T.replace "normtruthbin" "\\sigma_"
      . T.cons '$'
      $ T.snoc s '$'
    else
      T.replace "__1" ""
      . T.replace "v2trk" "trk"
      $ T.replace "_" " " s
