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
import           Atlas.CrossSections
import           BFrag.Systematics      (lumi)
import           Control.Applicative    (liftA2)
import           Control.Arrow          ((&&&))
import           Control.Lens
import           Data.HashMap.Strict    (HashMap)
import qualified Data.HashMap.Strict    as HM
import qualified Data.Histogram.Generic as H
import           Data.List              (sort, transpose)
import           Data.Maybe             (fromJust, fromMaybe)
import           Data.Monoid            (Sum (..))
import qualified Data.Text              as T
import           Data.Vector            (Vector, (!))
import qualified Data.Vector            as V
import           GHC.Exts               (IsString)
import           Model
import           RunModel
import           System.Environment     (getArgs)


type TextMap = HashMap T.Text

matrixname, recohname, recomatchhname, truehname :: IsString s => s
matrixname = "/elmujjmatched/zbtcmig"
recohname = "/elmujj/probejets/zbtc"
recomatchhname = "/elmujjmatched/probejets/zbtc"
truehname = "/elmujjtrue/truejets/zbtc"

regex :: String
regex = matrixname ++ "|" ++ recohname ++ "|" ++ recomatchhname ++ "|" ++ truehname


binMerges :: [[Int]]
binMerges =
  [ [00, 01, 02]
  , [03, 04, 05]
  , [06, 07, 08]
  , [09, 10, 11]
  , [12, 13, 14]
  , [15, 16]
  , [17, 18]
  , [19, 20]
  ]

trimTrueV :: Monoid a => Vector a -> Vector a
trimTrueV = mergeV mappend mempty binMerges

trimTrueD :: Vector Double -> Vector Double
trimTrueD = fmap getSum . trimTrueV . fmap Sum

trimTrueB :: ArbBin a -> ArbBin a
trimTrueB bs = foldl (flip $ uncurry mergeBinRange) bs . reverse $ (head &&& last) <$> binMerges

trimTrueH :: Obj -> Obj
trimTrueH =
  over _H1DD
  $ over histData trimTrueV
    . over bins trimTrueB

  -- rebinV 2 . drop 1
  -- V.fromList
  -- [ v ! 00 + v ! 01 + v ! 02
  -- , v ! 03 + v ! 04 + v ! 05
  -- , v ! 06 + v ! 07 + v ! 08
  -- , v ! 09 + v ! 10 + v ! 11
  -- , v ! 12 + v ! 13 + v ! 14
  -- , v ! 15 + v ! 16
  -- , v ! 17 + v ! 18
  -- , v ! 19 + v ! 20
  -- ]


-- TODO
-- partial!
main :: IO ()
main = do
  (xsecfile:outfile:infs) <- getArgs
  xsecs <- readXSecFile xsecfile
  procs <- decodeFiles (Just regex) infs

  let hs =
        case procs of
          Left s  -> error s
          Right x -> x

      (Sum ttw, ttnom) = hs ^?! ix (ProcessInfo 410501 FS)

      (nombkg', nommat') = toModel . fromJust . flip view hs . at $ ProcessInfo 410501 FS
      (afbkg, afmat) = toModel . fromJust . flip view hs . at $ ProcessInfo 410501 AFII
      (radbkg, radmat) = toModel . fromJust . flip view hs . at $ ProcessInfo 410511 AFII
      (mebkg, memat) = toModel . fromJust . flip view hs . at $ ProcessInfo 410225 AFII
      (psbkg, psmat) = toModel . fromJust . flip view hs . at $ ProcessInfo 410525 AFII

      bkgVar
        :: Vars (Vector Double)
        -> Vars (Vector Double)
        -> Vars (Vector Double)
        -> Vector Double
      bkgVar nom nom' var =
        V.zipWith (+) (nom ^. nominal)
        $ V.zipWith (-) (nom' ^. nominal) (var ^. nominal)

      bkg :: Vars (Vector Double)
      bkg =
        nombkg'
        & variations . at "PSUp" ?~ bkgVar nombkg' afbkg psbkg
        & variations . at "MEUp" ?~ bkgVar nombkg' afbkg mebkg
        & variations . at "RadUp" ?~ bkgVar nombkg' afbkg radbkg
        & (fmap.fmap) (*xsec)

      migVar
        :: Vars (Vector (Vector Double))
        -> Vars (Vector (Vector Double))
        -> Vars (Vector (Vector Double))
        -> Vector (Vector Double)
      migVar nom nom' var =
        V.zipWith add (nom ^. nominal)
        $ V.zipWith sub (nom' ^. nominal) (var ^. nominal)
        where
          add = V.zipWith (+)
          sub = V.zipWith (-)

      mat :: Vars (Vector (Vector Double))
      mat =
        nommat'
        & variations . at "PSUp" ?~ migVar nommat' afmat psmat
        & variations . at "MEUp" ?~ migVar nommat' afmat memat
        & variations . at "RadUp" ?~ migVar nommat' afmat radmat


      recoh = fmap (*(xsec/ttw)) . getH1DD . view nominal $ ttnom ^?! ix recohname
      trueh =
        fmap (*(xsec/ttw)) . trimTrueD . getH1DD . view nominal
        $ ttnom ^?! ix truehname

      datakey = ProcessInfo 0 DS

      dh :: Maybe (Vector Int)
      dh = fmap round . getH1DD . view nominal . (^?! ix recohname) . snd
            <$> (hs ^? ix datakey)

      xsec = xsecs ^?! _Just . ix 410501 . _1

      datah :: Vector Int
      datah = flip fromMaybe dh $ round . (* view nominal lumi) <$> recoh

      (model, params) = buildModel trueh mat (HM.singleton "ttbar" <$> bkg)


  putStrLn "\nmodel:"
  print model

  putStrLn "data:"
  print datah

  unfolded' <- runModel 1000 outfile datah model params

  let unfolded'' =
        sort . (fmap.fmap) (lmvskMean &&& lmvskVariance)
        . filter (T.isInfixOf "normtruth" . fst)
        $ HM.toList unfolded'

  print unfolded''


toModel
  :: (Sum Double, Folder (Vars YodaObj))
  -> (Vars (Vector Double), Vars (Vector (Vector Double)))
toModel (Sum w, hs) =
  let filt =
        liftSM . HM.filterWithKey
        $ \i _ -> not $ T.isInfixOf "down" i || T.isInfixOf "Down" i

      recoh' = getH1DD <$> hs ^?! ix recohname & variations %~ filt
      trueh' = trimTrueD . getH1DD <$> hs ^?! ix truehname & variations %~ filt
      recomatchh' = getH1DD <$> hs ^?! ix recomatchhname & variations %~ filt

      transposeV =
        V.fromList . fmap V.fromList . transpose . fmap V.toList . V.toList

      math' =
        transposeV . fmap trimTrueD . transposeV . getH2DD
        <$> hs ^?! ix matrixname & variations %~ filt

      math = math' <&> (fmap.fmap) (/w)
      trueh = trueh' <&> fmap (/w)
      recoh = recoh' <&> fmap (/w)
      recomatchh = recomatchh' <&> fmap (/w)
      bkgrecoh = V.zipWith (-) <$> recoh <*> recomatchh

  in (bkgrecoh, normmat trueh math)

  where
    normmat = liftA2 (V.zipWith (\x v -> (/x) <$> v))



buildModel
  :: Vector Double
  -> Vars (Vector (Vector Double))
  -> Vars (TextMap (Vector Double))
  -> (Model Double, TextMap (ModelParam Double))
buildModel trueH matH bkgHs = (nommod, params)
  where
    emptysig :: Vector Double
    emptysig = 0 <$ trueH

    mats = (fmap.fmap) (\x -> if x < 0 then 0 else x) <$> matH

    nommod =
      Model
        (view nominal bkgHs)
        emptysig
        (view nominal mats)
        (view nominal lumi)

    lumiparam =
      HM.singleton "LumiUp" . ModelParam 1.0 (LogNormal 1.0 1.0)
      $ ModelVar Nothing Nothing Nothing (lumi ^. variations . at "LumiUp")

    matparams =
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
      . flip imap trueH
      $ \i x ->
        ( T.pack $ "truthbin" ++ show i
        , ModelParam x Flat
          $ ModelVar Nothing (Just (emptysig & ix i .~ 1)) Nothing Nothing
        )

    params = matparams `mappend` trueparams `mappend` lumiparam

    systify v = fmap Just v & nominal .~ Nothing


getH1DD :: Annotated Obj -> Vector Double
getH1DD (Annotated _ (H1DD h)) =
  views histData (fmap (view sumW)) h
getH1DD _ = error "attempting to get H1DD from a different YodaObj."

-- TODO
-- partial!
getH2DD :: Annotated Obj -> Vector (Vector Double)
getH2DD (Annotated _ (H2DD h)) =
  (fmap.fmap) (view sumW)
  -- TODO
  -- not positive about the orientation here
  . V.fromList
  . fmap (view histData . snd)
  $ H.listSlicesAlongY h
getH2DD _ = error "attempting to get H2DD from a different YodaObj."


mergeV :: (a -> a -> a) -> a -> [[Int]] -> Vector a -> Vector a
mergeV f x ks v =
  V.fromList
  $ go <$> ks
  where
    go is = foldl f x $ (v !) <$> is

rebinV :: Int -> (a -> a -> a) -> Vector a -> Vector a
rebinV 0 _ v = v
rebinV 1 _ v = v
rebinV k f v =
  let n = length v
      m = n `div` k
      m' = n `mod` k
      v' = V.generate m (\i -> foldl1 f $ V.slice (i*k) k v)
  in case m' of
    0 -> v'
    l -> V.snoc v' . foldl1 f $ V.slice (m*k) l v
