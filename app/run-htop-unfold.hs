{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

-- TODO
-- there is a ton of partial stuff in here.
import           Atlas
import           Atlas.ProcessInfo
import           Atlas.ToYoda
import           BFrag.Systematics
import           Control.Applicative    (liftA2)
import           Control.Arrow          (first, second)
import           Control.Lens
import qualified Data.Histogram.Generic as H
import qualified Data.IntMap.Strict     as IM
import qualified Data.Map.Strict        as M
import qualified Data.Text              as T
import qualified Data.Vector            as V
import           Debug.Trace
import           Model
import           RunModel
import           System.IO              (hFlush, stdout)


type TextMap a = M.Map T.Text a

main :: IO ()
main = mainWith tmp

tmp :: Double -> String -> ProcMap (Folder (Vars YodaObj)) -> IO ()
tmp lu outfile procs = do
  mapMOf_ (ix 410000.to (M.keys.folderToMap).traverse) print procs
  let procst =
        M.fromList
        . fmap (first processTitle . second folderToMap)
        . IM.toList
        . ttbarSysts
        $ sans 0 procs

      nsamps = 100000
      recohname = "/elmujj/probejets/4psvtrks/ptgt30/matched/zbt"
      truehname = "/elmujj/truthjets/zbt"
      -- TODO
      -- partial!
      ttbarrecoh = trace "ttbarrecoh" $ procst ^?! ix "Pow+Py (nominal)" . ix recohname
      ttbartrueh = trace "ttbartrueh" $ procst ^?! ix "Pow+Py (nominal)" . ix truehname
      ttbarmath = procst
        ^?! ix "Pow+Py (nominal)"
          . ix "/elmujj/probejets/4psvtrks/ptgt30/matched/recobfragvstruebfrag"
      bkgHs = (^?! ix recohname) <$> sans "Pow+Py (nominal)" procst
      (dataH, model, params) =
        buildModel
          lu
          ttbarrecoh
          ttbartrueh
          ttbarmath
          bkgHs
          -- TODO
          -- the bottom is real data.
          (over (noted._H1DD) (scaling 38000) $ ttbarrecoh ^. nominal)
          -- (ttbarrecoh ^?! variations . ix "PowPyRadDown")
          -- (procs ^?! ix 0 . to folderToMap . ix recohname . nominal)

  putStrLn "data:"
  print dataH
  putStrLn "\nmodel:"
  print model
  putStrLn "\nvariations:"
  imapM_ (\i x -> print i >> views mpVariation print x) params
  hFlush stdout
  runModel nsamps outfile dataH model params

buildModel
  :: Double
  -> Vars YodaObj
  -> Vars YodaObj
  -> Vars YodaObj
  -> TextMap (Vars YodaObj)
  -> YodaObj
  -> (V.Vector Int, Model Double, TextMap (ModelParam Double))
buildModel lu recoH trueH matH bkgHs dataH =
  let
      -- TODO
      -- TODO
      -- currently cutting off last truth and reco bins!
      vtrue = getH1DD <$> trueH
      vreco = getH1DD <$> recoH
      vdata = floor <$> getH1DD dataH
      vvmat = getH2DD <$> matH
      vbkgs = fmap getH1DD <$> bkgHs

      -- nreco = traceShowId . trace "nreco" $ views nominal length vreco'
      -- vreco = V.slice 0 (nreco-1) <$> vreco'
      -- vvmat' = fmap (V.slice 0 (nreco-1)) . getH2DD <$> matH
      -- vdata = fmap (floor . (*lu)) . V.slice 0 (nreco-1) $ getH1DD dataH
      -- vbkgs = fmap (V.slice 0 (nreco-1) . getH1DD) <$> bkgHs

      -- vtrue = rebin 2 (+) <$> vtrue'
      -- vvmat = rebin 2 (V.zipWith (+)) <$> vvmat'

      emptysig = const 0 <$> nom vtrue

      signalreco =
        traceShowId . trace "signalreco"
        $ foldl (V.zipWith (+)) (const 0 <$> nom vreco) <$> vvmat

      -- TODO
      -- TODO
      -- how are we getting negative numbers in the bkgs?
      -- aMC has negative weights...
      bkgreco =
        traceShowId . trace "bkgreco"
        . (fmap.fmap) (\x -> if x < 0 then 0 else x)
        $ V.zipWith (-) <$> vreco <*> signalreco

      mvbkgs = sequenceA $ vbkgs & at "ttbarlight" ?~ bkgreco

      -- TODO
      -- TODO
      -- how are we getting negative numbers in the bkgs?
      -- aMC has negative weights...
      mats =
        (fmap.fmap) (\x -> if x < 0 then 0 else x)
        <$> normmat vtrue vvmat


      nommod =
        Model
          (nom mvbkgs)
          emptysig
          (nom mats)
          -- TODO
          -- lumi uncertainty
          lu

      sysparams =
        view variations
        . fmap (ModelParam 0.0 (Normal 0.0 1.0))
        $ ModelVar
          <$> systify mvbkgs
          <*> pure Nothing
          <*> systify mats
          <*> pure Nothing

      trueparams =
        M.fromList
        . V.toList
        . flip imap (nom vtrue)
        $ \i x ->
          ( T.pack $ "truthbin" ++ show i
          , ModelParam x Flat
            $ ModelVar Nothing (Just (emptysig & ix i .~ 1)) Nothing Nothing
          )

      params = M.union sysparams trueparams

  in (vdata, nommod, params)

  where
    -- TODO
    -- partial!
    getH1DD (Annotated _ (H1DD h)) =
      views histData (fmap (view sumW)) h
    getH1DD _ = error "attempting to get H1DD from a different YodaObj."

    -- TODO
    -- partial!
    getH2DD (Annotated _ (H2DD h)) =
      (fmap.fmap) (view sumW)
      -- TODO
      -- not positive about the orientation here
      . V.fromList
      . fmap (view histData . snd)
      $ H.listSlicesAlongY h
    getH2DD _ = error "attempting to get H2DD from a different YodaObj."

    nom = view nominal
    systify v = fmap Just v & nominal .~ Nothing

    normmat = liftA2 (V.zipWith (\x v -> (/x) <$> v))

rebin :: Int -> (a -> a -> a) -> V.Vector a -> V.Vector a
rebin 0 _ v = v
rebin 1 _ v = v
rebin k f v =
  let n = length v
      m = n `div` k
      m' = n `mod` k
      v' = V.generate m (\i -> foldl1 f $ V.slice (i*k) k v)
  in case m' of
    0 -> v'
    l -> V.snoc v' . foldl1 f $ V.slice (m*k) l v

--   let preds = sans 0 procs
--
--       systttbarDSIDs = (+410000) <$> [1, 2, 3, 4] :: [Int]
--       (systttbar', systpreds) =
--         IM.partitionWithKey (\k _ -> k `elem` systttbarDSIDs) preds
--
--       -- the "nominal" variations from the ttbar systematic samples
--       -- are actually variations
--       systttbar = over (traverse.traverse) (view nominal) systttbar'
--
--       -- TODO
--       -- partial!
--       ttbar' = systpreds ^?! ix 410000
--       dat = (fmap.fmap) (view nominal) $ procs ^?! at datadsid
--
--
--       bkgs =  fold $ sans 410000 systpreds
--
--       ttbar :: Folder (Vars YodaObj)
--       ttbar =
--         foldr (\(k, fs) fv ->
--           inF2 (M.intersectionWith (\s v -> v & at k ?~ s)) fs fv) ttbar'
--           $ fmap (first (procDict IM.!))
--             . IM.toList
--             $ systttbar
--
--   in (mappend ttbar bkgs, dat)
--
-- procDict :: IM.IntMap T.Text
-- procDict =
--   [ (410000, "PowPyNom")
--   , (410001, "PowPyRadUp")
--   , (410002, "PowPyRadDown")
--   , (410003, "aMCHer")
--   , (410004, "PowHer")
--   ]
