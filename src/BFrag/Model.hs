{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module BFrag.Model where

import           Atlas
import Data.Functor.Apply
import           Data.StrictMap
import           Data.Biapplicative
import           BFrag.Systematics
import           Control.Applicative (liftA2)
import           Control.Arrow       ((***))
import           Control.Lens hiding (_1, _2)
import           Data.Foldable       (fold)
import Data.StrictHashMap
import           Data.List           (partition)
import qualified Data.Map            as M
import           Data.Maybe          (fromMaybe)
import           Data.Semigroup      ((<>))
import qualified Data.Text           as T
import           GHC.Exts            (toList)
import Data.Annotated
import Both
import Optic (hubble)


type FA a = Folder (Annotated a)

-- bfragModel
--   :: Maybe String
--   -> StrictMap ProcessInfo AnaObjs
--   -> Either' String
--       ( Both (FA Histo1D) (FA Histo2D)
--       , AnaObjs
--       , AnaObjs
--       , StrictMap ProcessInfo (Both (Folder Histo1D) (Folder Histo2D))
--       )
-- 
-- bfragModel stresstest procs = do
--   zjets' <- getProcs procs zjetskeys
--   diboson' <- getProcs procs dibosonkeys
--   stopdr' <- getProcs procs stopdrkeys
--   stopds' <- onlyNominal <$> getProcs procs stopdskeys
--   nom' <- getProcs procs [nomkey]
-- 
--   afii <- onlyNominal <$> getProcs procs [afiikey]
--   radup <- onlyNominal <$> getProcs procs [radupkey]
--   raddown <- onlyNominal <$> getProcs procs [raddownkey]
--   fsrup <- onlyNominal <$> getProcs procs [fsrupkey]
--   fsrdown <- onlyNominal <$> getProcs procs [fsrdownkey]
--   ps <- onlyNominal <$> getProcs procs [powh7key]
-- 
--   -- me <-
--   --   getProcs procs [amcpy8key] & (traverse.traverse.noted) %~ view nominal
-- 
-- 
--   -- sherpa <-
--   --   mappend nonttpred
--   --   <$> getProcs procs [sherpakey] & (traverse.traverse.noted) %~ view nominal
-- 
-- 
--   let zjets :: AnaObjs
--       zjets = zjets' & modVarAO "ZJetsNormUp" (*1.3)
-- 
--       diboson = diboson' & modVarAO "DibosonNormUp" (*1.3)
-- 
--       stop :: AnaObjs
--       stop =
--         setVarAO "STopDS" stopdr' stopds'
--         & modVarAO "STopNormUp" (*1.3)
-- 
-- 
--       nonttpred :: AnaObjs
--       nonttpred = bimap recoFilt recoFilt $ fold [zjets, stop, diboson]
--         where
--           recoFilt = liftSM (M.filterWithKey (\k _ -> filtReco k))
-- 
--           filtReco k =
--             not . any ($ k)
--             $ [ T.isInfixOf "/elmujjmatched/"
--               , T.isInfixOf "/elmujjtrue/"
--               ]
-- 
-- 
--       nom :: AnaObjs
--       nom = nom' & modVarAO "TTBarNormUp" (*1.20)
-- 
--       nomnom = onlyNominal nom
-- 
--       afiidiff = biliftF2 (-) nomnom afii
-- 
--       halfdiff u d = (u - d) / 2
--       raddiff = liftAO2 halfdiff radup raddown
--       fsrdiff = liftAO2 halfdiff fsrup fsrdown
-- 
--       -- me' = mappend me afiidiff
-- 
--       ps' = mappend ps afiidiff
--       rad' = mappend nomnom raddiff
--       fsr' = mappend nomnom fsrdiff
-- 
--       fullpred :: AnaObjs
--       fullpred =
--         liftAO collapseVars
--         . mappend nonttpred
--         . flip (setVarAO "RadUp") rad'
--         . flip (setVarAO "FSRUp") fsr'
--         . flip (setVarAO "PSUp") ps'
--         $ nom
-- 
-- 
--       data' =
--         case stresstest of
--           Nothing ->
--             either error ((fmap.fmap) (view nominal)) $ getProcs procs [datakey]
--           Just "closure" ->
--             (fmap.fmap) (scaleO $ view nominal lumi) nomnom
--           Just var ->
--             let vs = (traverse.traverse) (view $ variations . at (T.pack var)) fullpred
--             in case vs of
--               Nothing -> error "missing stress test variation"
--               Just fo -> (fmap.fmap) (scaleO $ view nominal lumi) fo
-- 
-- 
--       ttprocs = inSM (HM.filterWithKey $ \k _ -> k `elem` ttkeys) procs
-- 
--       ttpred =
--         (fmap.fmap) (view nominal) . mappend nonttpred
--         <$> strictMap ttprocs
-- 
-- 
--   return (data', fullpred, nonttpred, ttpred)
-- 
--   where
--     toEither' s Nothing  = Left' s
--     toEither' _ (Just x) = Right' x
-- 
-- 
--     onlyNominal :: AnaObjs -> Both (Folder Histo1D) (Folder Histo2D)
--     onlyNominal = liftAO . fmap . view $ noted.nominal
-- 
-- 
--     setVarAO s = biliftF2 (setVar s)
--       where setVar s n v = n & noted.variations.at s ?~ v
-- 
--     modVarAO s f n = bimap (modVar s (fmap f)) (modVar s (fmap.fmap $ f))
--       where modVar s f n = setVar s n (f n)
-- 
--     liftF f = liftSM $ M.map f
--     liftF2 f = liftSM2 $ M.intersectionWith f
-- 
--     biliftF f = bimap (liftF f) (liftF f)
--     biliftF2 f = biliftA2 (liftF2 f) (liftF2 f)
-- 
--     liftFA = liftSM . M.map . fmap
--     liftFA2 = liftSM2.M.intersectionWith.liftA2
-- 
--     biliftFA f = bimap (liftFA f) (liftFA f)
--     biliftFA2 f = biliftA2 (liftFA2 f) (liftFA2 f)
-- 
-- 
--     biliftAO f =
--       bimap
--         ((liftFA.fmap.fmap) f)
--         ((liftFA.fmap.fmap.fmap) f)
-- 
--     biliftAO2 f =
--       biliftA2
--         ((liftFA2.liftA2.binIntersectionWith) f)
--         ((liftFA2.liftA2.binIntersectionWith.binIntersectionWith) f)
-- 
-- 
--     getProcs
--       :: StrictMap ProcessInfo AnaObjs
--       -> [ProcessInfo]
--       -> Either' String AnaObjs
--     getProcs hs pis = do
--       let getProc p = toEither' ("missing " ++ show p) $ view (at p) hs
--       hs' <- traverse getProc pis
--       return $ mconcat hs'
-- 
-- 
--     vardiff nom varup vardown =
--       (<>) nom . scaleO 0.5 $ corrDiffO varup vardown
-- 
--     collapseVars :: Vars a -> Vars a
--     collapseVars (Variation n vs) =
--       let vs' = toList vs
--           filt s = T.isInfixOf (T.toLower s) . T.toLower
-- 
--           rmEnd s =
--             fmap $ \(x, y) ->
--               let x' = T.toLower x
--                   s' = T.toLower s
--                   x'' = fromMaybe x' $ T.stripSuffix s' x'
--               in (x'', y)
-- 
--           (downs, ups) =
--             ((HM.fromList . rmEnd "down") *** (HM.fromList . rmEnd "up"))
--             $ partition (filt "down" . fst) vs'
-- 
--       in Variation n . strictMap
--           -- remove systs from LOOSE tracks.
--           . HM.filterWithKey (\k _ -> not $ "loose" `T.isInfixOf` k || "npvtrksf" `T.isInfixOf` k)
--           $ HM.unionWith (vardiff n) ups downs
-- 
--     ttkeys =
--       [ nomkey, afiikey, radupkey, raddownkey, fsrupkey, fsrdownkey, amcpy8key
--       , powh7key, sherpakey, powp6key
--       ]


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
