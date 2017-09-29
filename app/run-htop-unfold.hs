{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

-- TODO
-- there is a ton of partial stuff in here.

import           Atlas
import           Atlas.CrossSections
import           BFrag.BFrag
import           BFrag.Systematics      (lumi)
import           Control.Applicative    (liftA2)
import           Control.Arrow          ((***))
import           Control.Lens
import           Data.HashMap.Strict    (HashMap)
import qualified Data.HashMap.Strict    as HM
import qualified Data.Histogram.Generic as H
import           Data.List              (sort)
import           Data.Maybe             (fromJust, fromMaybe)
import           Data.Monoid            (Sum (..))
import           Data.Semigroup         ((<>))
import           Data.TDigest           (quantile)
import qualified Data.Text              as T
import           Data.Vector            (Vector)
import qualified Data.Vector            as V
import           Model
import           RunModel
import           System.Environment     (getArgs)
import           System.IO              (BufferMode (..), IOMode (..),
                                         hPutStrLn, hSetBuffering, stdout,
                                         withFile)


type TextMap = HashMap T.Text

type H1DI = Histogram V.Vector (ArbBin Double) Int
type H1DD = Histogram V.Vector (ArbBin Double) Double
type H2DD = Histogram V.Vector (Bin2D (ArbBin Double) (ArbBin Double)) Double

type H1D = Hist1D (ArbBin Double)
type H2D = Hist2D (ArbBin Double) (ArbBin Double)

unsafeHAdd h h' = fromJust $ hzip' (+) h h'
unsafeHSub h h' = fromJust $ hzip' (-) h h'

regex :: String
regex = matrixname ++ "|" ++ recohname ++ "|" ++ recomatchhname ++ "|" ++ truehname


main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  (xsecfile:outfile:youtfolder:infs) <- getArgs
  xsecs <- readXSecFile xsecfile
  procs <- decodeFiles (Just regex) infs

  let hs =
        case procs of
          Left s  -> error s
          Right x -> imap norm x


      -- TODO
      -- only handling ttbar right now.
      xsec = xsecs ^?! _Just . ix 410501 . _1

      norm (ProcessInfo _ DS) (_, hs') = hs'
      norm _ (Sum w, hs')              = (fmap.fmap) (scaleYO (xsec/w)) <$> hs'

      nomkey = ProcessInfo 410501 FS
      afkey = ProcessInfo 410501 AFII
      radkey = ProcessInfo 410511 AFII
      mekey = ProcessInfo 410225 AFII
      pskey = ProcessInfo 410525 AFII
      datakey = ProcessInfo 0 DS

      getProc :: ProcessInfo -> (Vars (Annotated H1DD), Vars (Annotated H2DD))
      getProc = toModel . fromJust . flip view hs . at

      (nombkg', nommat') = getProc nomkey
      (afbkg, afmat) = (view nominal *** view nominal) . getProc $ afkey
      (radbkg, radmat) = (view nominal *** view nominal) . getProc $ radkey
      (mebkg, memat) = (view nominal *** view nominal) . getProc $ mekey
      (psbkg, psmat) = (view nominal *** view nominal) . getProc $ pskey

      vardiff nom nom' var = unsafeHAdd nom $ unsafeHSub var nom'

      bkg =
        let nom = view nominal nombkg'
            go x = vardiff <$> nom <*> afbkg <*> x
        in
          nombkg'
          & variations . at "PSUp" ?~ go psbkg
          & variations . at "MEUp" ?~ go mebkg
          & variations . at "RadUp" ?~ go radbkg

      mat =
        let nom = view nominal nommat'
            go x = vardiff <$> nom <*> afmat <*> x
        in
          nommat'
          & variations . at "PSUp" ?~ go psmat
          & variations . at "MEUp" ?~ go memat
          & variations . at "RadUp" ?~ go radmat

      matdiffs :: StrictMap T.Text (Annotated H2D)
      matdiffs =
        let n = view nominal mat
            vs = view noted <$> view variations mat
        in (fmap.fmap) doubToDist2D . (\h -> unsafeHSub h <$> n) <$> vs

      doubToDist2D :: Double -> Dist2D Double
      doubToDist2D w = filling (Pair 0 0) w mempty

      putMatDiff t ao =
        withFile (youtfolder <> "/" <> T.unpack t <> ".yoda") WriteMode $ \h ->
          hPutStrLn h . T.unpack . printYodaObj ("/htop" <> matrixname <> "diff")
          $ H2DD <$> ao



      recoh, trueh :: H1DD
      recoh =
        fmap (view sumW) . trimRecoH
        $ hs ^?! ix nomkey . ix recohname . nominal . noted . _H1DD
      trueh =
        fmap (view sumW) . trimTrueH
        $ hs ^?! ix nomkey . ix truehname . nominal . noted . _H1DD

      datah :: H1DI
      datah =
        fmap round
        . maybe ((*view nominal lumi) <$> recoh) (fmap (view sumW) . trimRecoH)
        $ hs ^? ix datakey . ix recohname . nominal . noted . _H1DD

      xs :: [(Double, (Double, Double))]
      xs =
        V.toList . fmap (\(mn, mx) -> ((mn+mx)/2, (mn, mx))) . binsList
        $ view bins trueh

      (model, params) =
        buildModel
          (view histData trueh)
          (getH2DD . view noted <$> mat)
          (HM.singleton "ttbar" . view histData . view noted <$> bkg)

  imapM_ putMatDiff matdiffs

  putStrLn "data:"
  print datah

  unfolded' <- runModel 10000 outfile (view histData datah) model params

  let unfolded'' =
        sort
        . fromMaybe (error "missing best fit values")
        . (traverse.traverse) quant
        . filter (T.isPrefixOf "truth" . fst)
        $ HM.toList unfolded'

      quant (mx, y) = do
        x <- mx
        q16 <- quantile 0.16 y
        q84 <- quantile 0.84 y
        return (x, (q16, q84))

  withFile (youtfolder <> "/htop.yoda") WriteMode $ \h ->
    hPutStrLn h . T.unpack . printScatter2D ("/REF/htop" <> truehname)
      $ zipWith (\x (_, y) -> (x, y)) xs unfolded''

  where
    scaleYO w (H1DD h) = H1DD $ scaling w h
    scaleYO w (H2DD h) = H2DD $ scaling w h
    scaleYO _ p        = p



toModel
  :: Folder (Vars YodaObj)
  -> (Vars (Annotated H1DD), Vars (Annotated H2DD))
toModel hs =
  let filt =
        liftSM . HM.filterWithKey
        $ \i _ -> not $ T.isInfixOf "down" i || T.isInfixOf "Down" i

      recoh, trueh, recomatchh :: Vars (Annotated H1DD)
      recoh =
        fmap (fmap (view sumW) . trimRecoH . fromJust . preview _H1DD)
        <$> hs ^?! ix recohname & variations %~ filt
      trueh =
        fmap (fmap (view sumW) . trimTrueH . fromJust . preview _H1DD)
        <$> hs ^?! ix truehname & variations %~ filt
      recomatchh =
        fmap (fmap (view sumW) . trimRecoH . fromJust . preview _H1DD)
        <$> hs ^?! ix recomatchhname & variations %~ filt

      math :: Vars (Annotated H2DD)
      math =
        fmap (fmap (view sumW) . H.liftY trimRecoH . H.liftX trimTrueH . fromJust . preview _H2DD)
        <$> hs ^?! ix matrixname
        & variations %~ filt

      bkgrecoh = liftA2 unsafeHSub <$> recoh <*> recomatchh

  in (bkgrecoh, liftA2 normmat <$> math <*> trueh)

  where
    normmat :: H2DD -> H1DD -> H2DD
    normmat m h = H.liftX (\hm -> fromJust $ hzip' (/) hm h) m



buildModel
  :: Vector Double
  -> Vars (Vector (Vector Double))
  -> Vars (TextMap (Vector Double))
  -> (Model Double, TextMap (ModelParam Double))
buildModel trueH matH bkgHs = (nommod, params)
  where
    emptysig = 0 <$ trueH

    mats = (fmap.fmap) (\x -> if x < 0 then 0 else x) <$> matH

    nommod =
      Model
        (view nominal bkgHs)
        emptysig
        (view nominal mats)
        (view nominal lumi)

    lumiparam =
      HM.singleton "LumiUp" . ModelParam 1.0 (LogNormal 0.0 0.1)
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

getH2DD :: H2DD -> Vector (Vector Double)
getH2DD h =
  V.fromList
  . fmap (view histData . snd)
  $ H.listSlicesAlongY h


printScatter2D
  :: T.Text
  -> [((Double, (Double, Double)), (Double, (Double, Double)))]
  -> T.Text
printScatter2D pa xys =
  T.unlines $
    [ "# BEGIN YODA_SCATTER2D " <> pa
    , "Type=Scatter2D"
    , "Path=" <> pa
    , "IsRef=1"
    ]
    ++
      fmap printPoint xys
      ++
      [ "# END YODA_SCATTER2D", ""
      ]

  where
    printPoint ((x, (xdown, xup)), (y, (ydown, yup))) =
      let area = xup - xdown
      in T.intercalate "\t" . fmap (T.pack . show)
        $ [x, x - xdown, xup - x, y/area, (y - ydown)/area, (yup - y)/area]



liftAnn2 :: (a -> b -> c) -> Annotated a -> Annotated b -> Annotated c
liftAnn2 f (Annotated ma a) (Annotated mb b) =
  Annotated (ma <> mb) (f a b)
