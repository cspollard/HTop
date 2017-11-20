{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import           Atlas
import           Atlas.CrossSections
import           BFrag.BFrag
import           BFrag.Model
import           BFrag.Systematics      (lumi)
import           Control.Applicative    (liftA2, liftA3)
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
unsafeHDiv h h' = fromJust $ hzip' (/) h h'

regex :: String
regex = zblcmatrixname ++ "|" ++ zblcrecohname ++ "|" ++ zblcrecomatchhname ++ "|" ++ zblctruehname


main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  (xsecfile:outfile:youtfolder:infs) <- getArgs
  xsecs <- fromMaybe (error "failed to read xsecs") <$> readXSecFile xsecfile
  procs <- either error id <$> decodeFiles (Just regex) infs

  let normedProcs = either error id $ itraverse (normToXsec xsecs) procs
      (data', pred', _, _) = either error id $ bfragModel normedProcs
      (bkg, migration) = unfoldingInputs pred'

      -- filtVar f = inSM (strictMap . HM.filter (f . view noted))
      --
      -- -- only keep bkg variations with a > 2% deviation
      -- bkgFilt hnom hvar =
      --   any go . view histData . fromJust
      --   $ hzip' f hnom hvar
      --   where
      --     f n v = (n, v - n)
      --     go (n, d) = abs (1 - d / n) > 0.02
      --
      -- -- only keep matrix variations with a deviation > 0.1% in a bin with > 0.1% efficiency
      -- matFilt hnom hvar =
      --   any go . view histData . fromJust
      --   $ hzip' f hnom hvar
      --   where
      --     f n v = (n, abs $ v - n)
      --     go (n, d) = n > 0.001 && d > 0.001

      migration' = (fmap.fmap.fmap) doubToDist2D migration

      matdiffs' :: Annotated (Vars (Maybe H2DD))
      matdiffs' =
        let tmp = fmap Just <$> migration
            vs = tmp & traverse.nominal .~ Nothing
            n = tmp & traverse.variations .~ mempty
        in (liftA2.liftA2.liftA2) unsafeHSub vs n

      matdiffs = (fmap.fmap.fmap.fmap) doubToDist2D matdiffs'

      matreldiffs :: Annotated (Vars (Maybe H2D))
      matreldiffs =
        let n = migration & traverse.variations .~ mempty & (fmap.fmap) Just
            divs = (liftA2.liftA2.liftA2) unsafeHDiv matdiffs' n
        in (fmap.fmap.fmap.fmap) doubToDist2D divs

      doubToDist2D :: Double -> Dist2D Double
      doubToDist2D w = filling (Pair 0 0) w mempty

      showMigMat n ao =
        printYodaObj ("/htop" <> n)
          $ H2DD . scaleByBinSize2D <$> ao

      writeMigs t (m, mdiff, mreldiff) =
        withFile (youtfolder <> "/" <> T.unpack t <> ".yoda") WriteMode $ \h ->
          hPutStrLn h . T.unpack . T.intercalate "\n\n"
          $ [ showMigMat (zblcmatrixname <> "eff") m
            , maybe "" (showMigMat $ zblcmatrixname <> "diff") mdiff
            , maybe "" (showMigMat $ zblcmatrixname <> "reldiff") mreldiff
            ]

      trueh = fmap (view sumW) . trimTrueH
        $ pred' ^?! ix zblctruehname . noted . nominal . _H1DD

      datah :: H1DI
      datah =
        fmap (round . view sumW) . trimRecoH
        $ data' ^?! ix zblcrecohname . noted . _H1DD

      xs :: [(Double, (Double, Double))]
      xs =
        V.toList . fmap (\(mn, mx) -> ((mn+mx)/2, (mn, mx))) . binsList
        $ view bins trueh

      (model, params) =
        buildModel
          (view histData trueh)
          (getH2DD <$> view noted migration)
          (HM.singleton "bkg" . view histData <$> view noted bkg)

  imapM_ writeMigs . variationToMap "nominal"
    $ liftA3 (,,)
      (sequence migration')
      (sequence <$> sequence matdiffs)
      (sequence <$> sequence matreldiffs)

  putStrLn "data:"
  views histData print datah

  putStrLn "model:"
  print model

  unfolded' <- runModel 100000 outfile (view histData datah) model params

  let unfolded'' =
        sort
        . fromMaybe (error "missing best fit values")
        . (traverse.traverse) quant
        . filter (T.isPrefixOf "truth" . fst)
        $ HM.toList unfolded'

      unfoldednorm =
        sort
        . fromMaybe (error "missing best fit values")
        . (traverse.traverse) quant
        . filter (T.isPrefixOf "normtruth" . fst)
        $ HM.toList unfolded'

      quant (mx, y) = do
        x <- mx
        q16 <- quantile 0.16 y
        q84 <- quantile 0.84 y
        return (x, (q16, q84))

  withFile (youtfolder <> "/htop.yoda") WriteMode $ \h ->
    do
      hPutStrLn h . T.unpack . printScatter2D ("/REF/htop" <> zblctruehname)
        $ zipWith (\x (_, y) -> (x, y)) xs unfolded''
      hPutStrLn h . T.unpack . printScatter2D ("/REF/htop" <> zblctruehname <> "norm")
        $ zipWith (\x (_, y) -> (x, y)) xs unfoldednorm


  where
    normToXsec
      :: CrossSectionInfo
      -> ProcessInfo
      -> (Sum Double, Folder (Vars YodaObj))
      -> Either String (Folder (Annotated (Vars Obj)))
    normToXsec _ (ProcessInfo _ DS) (_, hs) = return $ sequenceA <$> hs
    normToXsec xsecs (ProcessInfo ds _) (Sum w, hs) = do
      xsec <-
        toEither ("missing cross section for dsid " ++ show ds)
        $ xsecs ^? ix ds . _1
      return $ sequenceA . (fmap.fmap) (scaleO (xsec/w)) <$> hs

    toEither s Nothing  = Left s
    toEither _ (Just x) = return x

    scaleO :: Double -> Obj -> Obj
    scaleO w (H1DD h) = H1DD $ scaling w h
    scaleO w (H2DD h) = H2DD $ scaling w h
    scaleO w (P1DD h) = P1DD $ scaling w h




unfoldingInputs
  :: Folder (Annotated (Vars Obj))
  -> (Annotated (Vars  H1DD), Annotated (Vars H2DD))
unfoldingInputs hs =
  let recoh, trueh, recomatchh, bkgrecoh :: Annotated (Vars H1DD)
      recoh =
        fmap (fmap (view sumW) . trimRecoH . (^?! _H1DD))
        <$> hs ^?! ix zblcrecohname
      trueh =
        fmap (fmap (view sumW) . trimTrueH . (^?! _H1DD))
        <$> hs ^?! ix zblctruehname
      recomatchh =
        fmap (fmap (view sumW) . trimRecoH . (^?! _H1DD))
        <$> hs ^?! ix zblcrecomatchhname

      math :: Annotated (Vars H2DD)
      math =
        fmap (fmap (view sumW) . H.liftY trimRecoH . H.liftX trimTrueH . fromJust . preview _H2DD)
        <$> hs ^?! ix zblcmatrixname

      bkgrecoh = liftA2 unsafeHSub <$> recoh <*> recomatchh

      normmat :: H2DD -> H1DD -> H2DD
      normmat m h = H.liftX (\hm -> fromJust $ hzip' (/) hm h) m

  in (bkgrecoh, liftA2 normmat <$> math <*> trueh)



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


scaleByBinSize2D
  :: (BinValue b ~ (Weight a, Weight a), IntervalBin b, Fractional (Weight a), Weighted a)
  => Histogram Vector b a -> Histogram Vector b a
scaleByBinSize2D h =
  let intervals = views bins binsList h
      go ((xmin, ymin), (xmax, ymax)) =
        scaling ((xmax - xmin) * (ymax - ymin))
  in over histData (V.zipWith go intervals) h
