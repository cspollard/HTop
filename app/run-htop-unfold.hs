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
import           Control.Applicative    (liftA2)
import           Control.Lens
import           Data.HashMap.Strict    (HashMap)
import qualified Data.HashMap.Strict    as HM
import qualified Data.Histogram.Generic as H
import qualified Data.IntMap.Strict     as IM
import           Data.Monoid            (Sum (..))
import qualified Data.Text              as T
import qualified Data.Vector            as V
import           Debug.Trace
import           GHC.Exts               (IsString)
import           Model
import           RunModel
import           System.Environment     (getArgs)


type TextMap = HashMap T.Text

matrixname, recohname, truehname :: IsString s => s
matrixname = "/htop/elmujjmatch/matched/4precosvtrks/recoptgt30/recozbtcvstruezbtc"
recohname = "/htop/elmujjmatch/matched/4precosvtrks/recoptgt30/probejets/zbt"
truehname = "/htop/elmujjmatch/matched/4precosvtrks/recoptgt30/truejets/zbttrue"

regex :: String
regex = matrixname ++ "|" ++ recohname ++ "|" ++ truehname

-- TODO
-- partial!
main :: IO ()
main = do
  (outfile:infs) <- getArgs
  procs <- decodeFiles (Just regex) infs
  let (Sum ttsumw, tt) =
        case procs of
          Left s  -> error s
          Right x -> x IM.! 410501

      ttbarrecoh = trace "ttbarrecoh" $ tt ^?! ix recohname . traverse . to getH1DD
      ttbartrueh = trace "ttbartrueh" $ getH1DD <$> tt ^?! ix truehname
      ttbarmath = trace "ttbarmath" $ getH2DD <$> tt ^?! ix matrixname

      (model, params) =
        buildModel
          (Variation 37000 [("LumiUp", 74000)])
          (over (traverse.traverse) (/ttsumw) ttbartrueh)
          ttbarmath
          mempty

      datah :: V.Vector Int
      datah = floor . (/ttsumw) . (*37000) <$> ttbarrecoh


  putStrLn "\nmodel:"
  print model

  putStrLn "data:"
  print datah

  runModel 10000 outfile datah model params


buildModel
  :: Vars Double
  -> Vars (V.Vector Double)
  -> Vars (V.Vector (V.Vector Double))
  -> Vars (TextMap (V.Vector Double))
  -> (Model Double, TextMap (ModelParam Double))
buildModel lu trueH matH bkgHs = (nommod, params)
  where
    emptysig :: V.Vector Double
    emptysig = const 0 <$> view nominal trueH

    mats =
      (fmap.fmap) (\x -> if x < 0 then 0 else x)
      <$> normmat trueH matH


    nommod =
      Model
        (view nominal bkgHs)
        emptysig
        (view nominal matH)
        (view nominal lu)

    sysparams =
      unSM
      . view variations
      . fmap (ModelParam 0.0 (Normal 0.0 1.0))
      $ ModelVar
        <$> systify bkgHs
        <*> pure Nothing
        <*> systify mats
        <*> pure Nothing

    trueparams =
      HM.fromList
      . V.toList
      . flip imap (nom trueH)
      $ \i x ->
        ( T.pack $ "truthbin" ++ show i
        , ModelParam x Flat
          $ ModelVar Nothing (Just (emptysig & ix i .~ 1)) Nothing Nothing
        )

    params = sysparams `mappend` trueparams

    nom = view nominal
    systify v = fmap Just v & nominal .~ Nothing

    normmat = liftA2 (V.zipWith (\x v -> (/x) <$> v))


getH1DD :: Annotated Obj -> V.Vector Double
getH1DD (Annotated _ (H1DD h)) =
  views histData (fmap (view sumW)) h
getH1DD _ = error "attempting to get H1DD from a different YodaObj."

-- TODO
-- partial!
getH2DD :: Annotated Obj -> V.Vector (V.Vector Double)
getH2DD (Annotated _ (H2DD h)) =
  (fmap.fmap) (view sumW)
  -- TODO
  -- not positive about the orientation here
  . V.fromList
  . fmap (view histData . snd)
  $ H.listSlicesAlongY h
getH2DD _ = error "attempting to get H2DD from a different YodaObj."

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

-- --   let preds = sans 0 procs
-- --
-- --       systttbarDSIDs = (+410000) <$> [1, 2, 3, 4] :: [Int]
-- --       (systttbar', systpreds) =
-- --         IM.partitionWithKey (\k _ -> k `elem` systttbarDSIDs) preds
-- --
-- --       -- the "nominal" variations from the ttbar systematic samples
-- --       -- are actually variations
-- --       systttbar = over (traverse.traverse) (view nominal) systttbar'
-- --
-- --       -- TODO
-- --       -- partial!
-- --       ttbar' = systpreds ^?! ix 410000
-- --       dat = (fmap.fmap) (view nominal) $ procs ^?! at datadsid
-- --
-- --
-- --       bkgs =  fold $ sans 410000 systpreds
-- --
-- --       ttbar :: Folder (Vars YodaObj)
-- --       ttbar =
-- --         foldr (\(k, fs) fv ->
-- --           inF2 (M.intersectionWith (\s v -> v & at k ?~ s)) fs fv) ttbar'
-- --           $ fmap (first (procDict IM.!))
-- --             . IM.toList
-- --             $ systttbar
-- --
-- --   in (mappend ttbar bkgs, dat)
-- --
-- -- procDict :: IM.IntMap T.Text
-- -- procDict =
-- --   [ (410000, "PowPyNom")
-- --   , (410001, "PowPyRadUp")
-- --   , (410002, "PowPyRadDown")
-- --   , (410003, "aMCHer")
-- --   , (410004, "PowHer")
-- --   ]
