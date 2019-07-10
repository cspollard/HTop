{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import           Atlas
import           Atlas.CrossSections
import           BFrag.BFrag
import           Control.Arrow ((>>>))
import           BFrag.Model
import           BFrag.Smooth
import           BFrag.Systematics      (lumi)
import           Control.Applicative    (liftA2, liftA3)
import           Control.Lens
import           Data.Char              (isNumber)
import           Data.Foldable          (fold)
import           Data.HashMap.Strict    (HashMap)
import qualified Data.HashMap.Strict    as HM
import qualified Data.Histogram.Generic as H
import           Data.List              (intercalate, nub, sort, sortBy, sortOn)
import           Data.Maybe             (fromJust, fromMaybe)
import           Data.Monoid            (Sum (..))
import           Data.Semigroup         ((<>))
import           Data.TDigest           (quantile)
import qualified Data.Text              as T
import           Data.Vector            (Vector)
import qualified Data.Vector            as V
import           Debug.Trace
import           Model
import           Numeric.Uncertain
import qualified Numeric.AD             as AD
import           Options.Applicative
import           RunModel               (latextable, runModel)
import           System.IO              (BufferMode (..), IOMode (..),
                                         hPutStrLn, hSetBuffering, stdout,
                                         withFile)
import           Text.Printf


type TextMap = HashMap T.Text

type H1D = Hist1D (ArbBin Double)
type H2D = Hist2D (ArbBin Double) (ArbBin Double)

type H1DI = Histogram V.Vector (ArbBin Double) Int
type H1DD = Histogram V.Vector (ArbBin Double) (Uncert Double)
type H2DD = Histogram V.Vector (Bin2D (ArbBin Double) (ArbBin Double)) (Uncert Double)


unsafeHAdd h h' = fromJust $ hzip (+) h h'
unsafeHSub h h' = fromJust $ hzip (-) h h'
unsafeHDiv h h' = fromJust $ hzip (/) h h'
unsafeHMul h h' = fromJust $ hzip (*) h h'

data Args =
  Args
    { mcmcfile   :: String
    , nsamples   :: Int
    , statOnly   :: Bool
    , yodafolder :: String
    , xsecfile   :: String
    , observable :: String
    , infiles    :: [String]
    , stresstest :: Maybe String
    } deriving (Show)

inArgs :: Parser Args
inArgs =
  Args
  <$> strOption
    ( long "mcmcfile" <> metavar "MCMCFILE" )
  <*> (
    option auto ( long "nsamples" <> metavar "NSAMPLES=10000" )
    <|> pure 10000
    )
  <*> switch (long "stat-only")
  <*> strOption
    ( long "yodafolder" <> metavar "YODAFOLDER" )
  <*> strOption
    ( long "xsecfile" <> metavar "XSECFILE" )
  <*> strOption
    ( long "observable" <> metavar "OBSERVABLE" )
  <*> some (strArgument (metavar "INFILES"))
  <*> optional
    ( strOption
      ( long "stresstest"
      <> help "unfold a variation as a stress test"
      )
    )


main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- execParser $ info (helper <*> inArgs) fullDesc

  let (recohname, truehname, _, matrixname) = obsNames (observable args)
      regex = intercalate "|" $ obsNames (observable args) ^.. each

  xsecs <- fromMaybe (error "failed to read xsecs") <$> readXSecFile (xsecfile args)
  procs <- either error id <$> decodeFiles (pure regex) empty (infiles args)

  let normedProcs = either error id $ itraverse (normToXsec xsecs) procs

      (data', pred', _, _) =
        either error id
        $ bfragModel (stresstest args) normedProcs

      (bkg, migration) = unfoldingInputs (observable args) pred'

      filtVar f v =
        let nom = view nominal v
        in over variations (inSM (strictMap . HM.filter (f nom))) v

      -- only keep bkg variations with a > 5% deviation
      bkgFilt hnom hvar =
        or . view histData . fromJust
        $ hzip' f hnom hvar
        where
          f n v =
            let d = abs (v - n) / n
            in d > 0.05

      -- only keep matrix variations with a deviation > 0.01%
      matFilt hnom hvar =
        or . view histData . fromJust
        $ hzip' f hnom hvar
        where
          f n v =
            let d = abs (v - n)
            in d > 0.0001

      matdiffs :: Annotated (Vars (Maybe H2DD))
      matdiffs =
        let tmp = fmap Just <$> migration
            vs = tmp & traverse.nominal .~ Nothing
            n = tmp & traverse.variations .~ mempty
        in (liftA2.liftA2.liftA2) unsafeHSub vs n

      matreldiffs :: Annotated (Vars (Maybe H2DD))
      matreldiffs =
        let n = migration & traverse.variations .~ mempty & (fmap.fmap) Just
        in (liftA2.liftA2.liftA2) unsafeHDiv matdiffs n

      showMigMat :: T.Text -> H2DD -> T.Text
      showMigMat n mat =
        printScatter3D ("/htop" <> n) False False
        . fmap convertBin3D
        . asList'
        $ uncertToScat <$> mat

      writeMigs :: T.Text -> (H2DD, Maybe H2DD, Maybe H2DD) -> IO ()
      writeMigs t (m, mdiff, mreldiff) =
        writeFile (yodafolder args <> "/" <> T.unpack t <> ".yoda")
          . T.unpack . T.intercalate "\n\n"
          $ [ showMigMat (matrixname <> "eff") m
            , maybe "" (showMigMat $ matrixname <> "diff") mdiff
            , maybe "" (showMigMat $ matrixname <> "reldiff") mreldiff
            ]
            ++ showEffSlicesY matrixname (m :: H2DD)
            ++ showEffSlicesX matrixname m
            ++ maybe [] (showEffSlicesY $ matrixname <> "diff") mdiff
            ++ maybe [] (showEffSlicesX $ matrixname <> "diff") mdiff

        where
          showEffSlicesX :: T.Text -> H2DD -> [T.Text]
          showEffSlicesX name m =
            ( \n (_, h) ->
                printScatter2D ("htop" <> name <> "effX" <> T.pack (show n)) False False
                . asList'
                $ uncertToScat <$> h
            )
            `imap` H.listSlicesAlongX m


          showEffSlicesY :: T.Text -> H2DD -> [T.Text]
          showEffSlicesY name m =
            ( \n (_, h) ->
                printScatter2D ("htop" <> name <> "effY" <> T.pack (show n)) False False
                . asList'
                $ uncertToScat <$> h
            )
            `imap` H.listSlicesAlongY m


      trueh = fmap (view sumW) . obsTruthTrimmers (observable args)
        $ pred' ^?! ix truehname . noted . nominal . _H1DD

      datah :: H1DI
      datah =
        fmap (round . view sumW) . obsRecoTrimmers (observable args)
        $ data' ^?! ix recohname . noted . _H1DD

      truebins, recobins :: [(Double, (Double, Double))]
      truebins =
        V.toList . fmap (\(mn, mx) -> ((mn+mx)/2, (mn, mx))) . binsList
        $ view bins trueh

      recobins =
        V.toList . fmap (\(mn, mx) -> ((mn+mx)/2, (mn, mx))) . binsList
        $ view bins datah

      (model, params) =
        buildModel
          (statOnly args)
          (view histData trueh)
          (getH2DD <$> filtVar matFilt (view noted migration))
          (HM.singleton "bkg" . fmap uMean . view histData <$> filtVar bkgFilt (view noted bkg))

  imapM_ writeMigs . variationToMap "nominal"
    $ liftA3 (,,)
      (view noted migration)
      (view noted matdiffs)
      (view noted matreldiffs)

  putStrLn "data:"
  views histData print datah

  putStrLn "model:"
  print model

  -- let regularization :: (Ord a, Floating a, AD.Mode a, AD.Scalar a ~ Double) => V.Vector a -> a
  --     regularization =
  --       if "rel" `T.isInfixOf` (T.pack $ observable args)
  --         then V.toList >>> zipWith (flip (/)) (AD.auto <$> normFactors trueh) >>> falling 2
  --         else const 0

  (unfolded', unfoldedcov) <-
    runModel (Just (5, 0.2)) (nsamples args) (mcmcfile args) (view histData datah) model (const 0) params

  let unfolded'' =
        fromMaybe (error "error getting mode or quantiles") . sequence
        $ quant <$> unfolded'

      byNum t = (read :: String -> Int) . T.unpack $ T.takeWhileEnd isNumber t

      reco' =
        sortOn (byNum . fst)
        . filter (T.isPrefixOf "reco" . fst)
        $ HM.toList unfolded''

      unfolded''' =
        sortOn (byNum . fst)
        . filter (T.isPrefixOf "truth" . fst)
        $ HM.toList unfolded''

      unfoldednorm =
        sortOn (byNum . fst)
        . filter (T.isPrefixOf "normtruth" . fst)
        $ HM.toList unfolded''

      quant (mx, y) = do
        x <- mx
        q16 <- quantile 0.16 y
        q50 <- quantile 0.50 y
        q84 <- quantile 0.84 y
        return (x, (q16, q50, q84))


  withFile (yodafolder args <> "/htop.yoda") WriteMode $ \h -> do
      hPutStrLn h . T.unpack . printScatter2D ("/REF/htop" <> truehname) True True
        $ zipWith (\x (_, (mode, (q16, _, q84))) -> (x, (mode, (q16, q84)))) truebins unfolded'''
      hPutStrLn h . T.unpack . printScatter2D ("/REF/htop" <> truehname <> "norm") True True
        $ zipWith (\x (_, (mode, (q16, _, q84))) -> (x, (mode, (q16, q84)))) truebins unfoldednorm
      hPutStrLn h . T.unpack . printScatter2D ("/htop" <> recohname) False True
        $ zipWith (\x (_, (mode, (q16, _, q84))) -> (x, (mode, (q16, q84)))) recobins reco'

  let symmetrize hm =
        let insert' h (x, y) = HM.insert (y, x) (h HM.! (x, y)) h
        in foldl insert' hm $ HM.keys hm

      uncerts =
        flip HM.mapMaybeWithKey (symmetrize unfoldedcov) $ \(name, name') cov ->
          let var =
                fromMaybe (error "missing variance")
                $ HM.lookup (name, name) unfoldedcov

              var' =
                fromMaybe (error "missing variance")
                $ HM.lookup (name', name') unfoldedcov

              mean =
                fromMaybe (error "missing best fit value") $ do
                  (_, (_, q50, _)) <- HM.lookup name unfolded''
                  return q50

              corr = cov / sqrt var / sqrt var'
              absuncert = abs $ cov / sqrt var'
              reluncert = absuncert / mean
          in
            if T.isPrefixOf "normtruthbin" name
                && not (T.isPrefixOf "truthbin" name')
                && not (T.isPrefixOf "recobin" name')
                && name' /= "llh"
              then Just (absuncert, reluncert, corr)
              else Nothing

  withFile (yodafolder args <> "/htop.stat") WriteMode $ \h -> do
    putStrLn "uncertainties and correlations:"
    print uncerts

    hPutStrLn h "absolute uncertainties:"
    hPutStrLn h . latextable $ view _1 <$> uncerts

    hPutStrLn h ""
    hPutStrLn h "relative uncertainties:"
    hPutStrLn h . latextable $ view _2 <$> uncerts

    hPutStrLn h ""
    hPutStrLn h "correlations:"
    hPutStrLn h . latextable $ view _3 <$> uncerts

  where
    normToXsec
      :: CrossSectionInfo
      -> ProcessInfo
      -> (Sum Double, Folder (Annotated (Vars Obj)))
      -> Either String (Folder (Annotated (Vars Obj)))
    normToXsec _ (ProcessInfo _ DS) (_, hs) = return hs
    normToXsec xsecs (ProcessInfo ds _) (Sum w, hs) = do
      xsec <-
        toEither ("missing cross section for dsid " ++ show ds)
        $ xsecs ^? ix ds . _1
      return $ (fmap.fmap) (scaleO $ xsec/w) <$> hs

    toEither s Nothing  = Left s
    toEither _ (Just x) = return x

    scaleO :: Double -> Obj -> Obj
    scaleO w (H1DD h) = H1DD $ scaling w h
    scaleO w (H2DD h) = H2DD $ scaling w h
    scaleO w (P1DD h) = P1DD $ scaling w h


unfoldingInputs
  :: String
  -> Folder (Annotated (Vars Obj))
  -> (Annotated (Vars  H1DD), Annotated (Vars H2DD))
unfoldingInputs obs hs =
  let (recohname, truehname, recomatchhname, matrixname)
        = obsNames obs

      trimR = obsRecoTrimmers obs
      trimT = obsTruthTrimmers obs

      recoh, trueh, recomatchh :: Annotated (Vars H1D)
      recoh =
        fmap (trimR . (^?! _H1DD))
        <$> hs ^?! ix recohname

      trueh =
        fmap (trimT . (^?! _H1DD))
        <$> hs ^?! ix truehname

      recomatchh =
        fmap (trimR . (^?! _H1DD))
        <$> hs ^?! ix recomatchhname

      math =
        fmap
          ( H.liftY (obsRecoTrimmers obs)
          . H.liftX (obsTruthTrimmers obs)
          . fromJust . preview _H2DD
          )
        <$> hs ^?! ix matrixname

      bkgrecoh :: Annotated (Vars H1DD)
      bkgrecoh =
        liftA2 (\h h' ->
          fromJust $ hzip (\d d' -> distToUncert (removeSubDist d d')) h h'
          )
        <$> recoh
        <*> recomatchh

      normmat m h = H.liftX (\hm -> fromJust $ hzip divCorr hm h) m

      mats = liftA2 normmat <$> math <*> trueh

      nommat :: H2DD
      nommat = view (noted.nominal) mats

      smooth = smoothRatioUncorr2DAlongXY nommat
      -- smooth = smoothRatioUncorr2DAlongY nommat

      -- can't this be simplified somehow? e.g. target several keys in
      -- one pass?
      mats' =
        mats
        & over (noted.variations.ix "ps") smooth
        & over (noted.variations.ix "fsr") smooth
        & over (noted.variations.ix "rad") smooth
        & over (noted.variations.ix "puwgt") smooth
        & over (noted.variations.ix "jet_21np_jet_flavor_composition__1") smooth
        & over (noted.variations.ix "jet_jer_single_np__1") smooth
        & over (noted.variations.ix "jet_21np_jet_pileup_rhotopology__1") smooth

  in (bkgrecoh, mats')



buildModel
  :: Bool
  -> Vector Double
  -> Vars (Vector (Vector Double))
  -> Vars (TextMap (Vector Double))
  -> (Model Double, TextMap (ModelParam Double))
buildModel statonly trueH matH bkgHs = (nommod, params)
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
      HM.singleton "lumi" . ModelParam 1.0 (LogNormal 0.0 0.021)
      $ ModelVar Nothing Nothing Nothing (lumi ^. variations . at "lumi")

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
        , ModelParam x NonNegative
          $ ModelVar Nothing (Just (emptysig & ix i .~ 1)) Nothing Nothing
        )

    params =
      if statonly
        then trueparams
        else matparams `mappend` trueparams `mappend` lumiparam

    systify v = fmap Just v & nominal .~ Nothing


getH2DD :: H2DD -> Vector (Vector Double)
getH2DD h =
  V.fromList
  . fmap (fmap uMean . view histData . snd)
  $ H.listSlicesAlongY h


printScatter2D
  :: T.Text
  -> Bool
  -> Bool
  -> [((Double, (Double, Double)), (Double, (Double, Double)))]
  -> T.Text
printScatter2D pa isRef rescale xys =
  T.unlines $
    [ "# BEGIN YODA_SCATTER2D " <> pa
    , "Type=Scatter2D"
    , "Path=" <> pa
    ]
    ++ (if isRef then ["IsRef=1"] else [])
    ++ fmap (printPoint rescale) xys
    ++ [ "# END YODA_SCATTER2D", "" ]

  where
    printPoint rescale ((x, (xdown, xup)), (y, (ydown, yup))) =
      let area = if rescale then xup - xdown else 1.0
      in T.intercalate "\t" . fmap (T.pack . show)
        $ [x, x - xdown, xup - x, y/area, (y - ydown)/area, (yup - y)/area]



printScatter3D
  :: T.Text
  -> Bool
  -> Bool
  -> [((Double, (Double, Double)), (Double, (Double, Double)), (Double, (Double, Double)))]
  -> T.Text
printScatter3D pa isRef rescale xyzs =
  T.unlines $
    [ "# BEGIN YODA_SCATTER3D " <> pa
    , "Type=Scatter3D"
    , "Path=" <> pa
    ]
    ++ (if isRef then ["IsRef=1"] else [])
    ++ fmap (printPoint rescale) xyzs
    ++ [ "# END YODA_SCATTER3D", "" ]

  where
    printPoint rescale ((x, (xdown, xup)), (y, (ydown, yup)), (z, (zdown, zup))) =
      let area = if rescale then (xup-xdown)*(yup-ydown) else 1.0
      in T.intercalate "\t" . fmap (T.pack . show)
        $ [ x, x-xdown, xup-x
          , y, y-ydown, yup-y
          , z/area, (z-zdown)/area, (zup-z)/area
          ]


scaleByBinSize2D
  :: (BinValue b ~ (Weight a, Weight a), IntervalBin b, Fractional (Weight a), Weighted a)
  => Histogram Vector b a -> Histogram Vector b a
scaleByBinSize2D h =
  let intervals = views bins binsList h
      go ((xmin, ymin), (xmax, ymax)) =
        scaling ((xmax - xmin) * (ymax - ymin))
  in over histData (V.zipWith go intervals) h


scaleByBinSize1D
  :: (BinValue b ~ Weight a, IntervalBin b, Fractional (Weight a), Weighted a)
  => Histogram Vector b a -> Histogram Vector b a
scaleByBinSize1D h =
  let intervals = views bins binsList h
      go (xmin, xmax) = scaling (xmax - xmin)
  in over histData (V.zipWith go intervals) h


distToUncert :: Floating a => DistND v a -> Uncert a
distToUncert DistND{..} = _sumW +/- sqrt _sumWW

uncertToScat :: Floating a => Uncert a -> (a, (a, a))
uncertToScat (x :+/- dx) = (x, (x-dx, x+dx))


divCorr :: (Eq a, Floating a) => DistND v1 a -> DistND v a -> Uncert a
divCorr num denom =
  let pw = _sumW num
      pw2 = _sumWW num
      nw = _sumW denom
      nw2 = _sumWW denom
      eff = pw / nw
      -- from https://root.cern.ch/doc/master/TH1_8cxx_source.html#l02871
      uncert = sqrt . abs $ ((1 - 2*pw/nw) * pw2 + sqr pw * nw2 / sqr nw) / sqr nw
  in if pw == nw
        then exact (if nw == 0 then 0 else 1)
        else eff +/- uncert
  where
    sqr x = x ^ (2::Int)



asList'
  :: IntervalBin bin
  => Histogram Vector bin a -> [((BinValue bin, (BinValue bin, BinValue bin)), a)]
asList' h =
  let b = view bins h
  in zip
      ((\i -> (H.fromIndex b i, H.binInterval b i)) <$> [0..])
      (V.toList $ view histData h)


convertBin3D
  :: (((Double, Double), ((Double, Double), (Double, Double))), (Double, (Double, Double)))
  -> (((Double, (Double, Double)), (Double, (Double, Double)), (Double, (Double, Double))))
convertBin3D (((x, y), ((xdown, ydown), (xup, yup))), zs)
  = ((x, (xdown, xup)), (y, (ydown, yup)), zs)


smoothRatioUncorr
  :: (Ord a, Floating a, BinValue bin ~ a, Bin bin)
  => Histogram Vector bin (Uncert a, Uncert a)
  -> Histogram Vector bin (Uncert a)
smoothRatioUncorr h = H.histogramUO (view bins h) Nothing (V.fromList smoothed)
  where
    lh = H.asList h

    rat = over (traverse._2) (\(nom, var) -> uMeanStd $ safeDiv var nom) lh
    (end, result) = polySmooth start 10 rat

    correction = exact . snd <$> result

    smoothed = zipWith (\(_, (nom, _)) corr -> nom*corr) lh correction

    safeDiv _ 0 = 1 +/- 1
    safeDiv 0 _ = 1 +/- 1
    safeDiv x y = x / y

    start :: Num a => [a]
    start = [1, 0, 0, 0]


smoothRatioUncorr2DAlongY
  :: (Ord a, Floating a, BinValue bY ~ a, BinEq bX, BinEq bY)
  => Histogram Vector (Bin2D bX bY) (Uncert a)
  -> Histogram Vector (Bin2D bX bY) (Uncert a)
  -> Histogram Vector (Bin2D bX bY) (Uncert a)
smoothRatioUncorr2DAlongY mnom mvar =
  H.liftY smoothRatioUncorr $ H.zip (,) mnom mvar



smoothRatioUncorr2D
  :: (Ord a, Floating a, Show a, Bin bX, BinValue bX ~ a, Bin bY, BinValue bY ~ a)
  => Histogram Vector (Bin2D bX bY) (Uncert a, Uncert a)
  -> Histogram Vector (Bin2D bX bY) (Uncert a)
smoothRatioUncorr2D h = H.histogramUO (view bins h) Nothing (V.fromList smoothed)
  where
    lh = H.asList h

    rat = over (traverse._2) (\(nom, var) -> uMeanStd $ safeDiv var nom) lh
    (end, result) = traceShowId $ polySmooth2D start (length start ^ 2) rat

    correction = exact . snd <$> result

    smoothed = zipWith (\(_, (nom, _)) corr -> nom*corr) lh correction

    safeDiv _ 0 = 1 +/- 1
    safeDiv 0 _ = 1 +/- 1
    safeDiv x y = x / y

    start :: Num a => [[a]]
    start =
      [ [1, 0, 0, 0, 0, 0]
      , [0, 0, 0, 0, 0, 0]
      , [0, 0, 0, 0, 0, 0]
      , [0, 0, 0, 0, 0, 0]
      , [0, 0, 0, 0, 0, 0]
      , [0, 0, 0, 0, 0, 0]
      ]


smoothRatioUncorr2DAlongXY
  :: (Ord a, Floating a, Show a, BinValue bY ~ a, BinValue bX ~ a, BinEq bX, BinEq bY)
  => Histogram Vector (Bin2D bX bY) (Uncert a)
  -> Histogram Vector (Bin2D bX bY) (Uncert a)
  -> Histogram Vector (Bin2D bX bY) (Uncert a)
smoothRatioUncorr2DAlongXY mnom mvar =
  smoothRatioUncorr2D $ H.zip (,) mnom mvar



normFactors :: (IntervalBin bin, Num (BinValue bin)) => Histogram Vector bin a -> [BinValue bin]
normFactors = asList' >>> fmap (views (_1._2) (\(x, y) -> (y-x)))


vdiff :: Num a => [a] -> [a]
vdiff [] = []
vdiff [_] = []
vdiff (x:xs@(y:_)) = x - y : vdiff xs


falling :: (Ord a, Num a) => a -> [a] -> a
falling weight = vdiff >>> foldMap go >>> getSum >>> (*weight)
  where
    go x = if x > 0 then mempty else Sum (abs x)
