{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import Linear.Vector
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
import           Data.Foldable          (fold, foldl')
import           Data.HashMap.Strict    (HashMap)
import qualified Data.HashMap.Strict    as HM
import qualified Data.Histogram.Generic as H
import           Data.List              (intercalate, nub, sort, sortBy, sortOn)
import           Data.Maybe             (fromMaybe)
import           Data.Monoid            (Sum (..))
import           Data.Semigroup         ((<>))
import           Data.TDigest           (quantile)
import qualified Data.Text              as T
import           Data.Vector            (Vector)
import qualified Data.Vector            as V
import           Debug.Trace
import           Model
import           Numeric.Uncertain hiding (liftU2)
import qualified Numeric.AD             as AD
import           Options.Applicative
import           RunModel               (latextable, runModel)
import           System.IO              (BufferMode (..), IOMode (..),
                                         hPutStrLn, hSetBuffering, stdout,
                                         withFile)
import           Text.Printf
import           Data.Functor.Compose


type TextMap = HashMap T.Text

type H1D = Hist1D (ArbBin Double)
type H2D = Hist2D (ArbBin Double) (ArbBin Double)

type H1DI = Histogram V.Vector (ArbBin Double) Int
type H1DD = Histogram V.Vector (ArbBin Double) (Uncert Double)
type H2DD = Histogram V.Vector (Bin2D (ArbBin Double) (ArbBin Double)) (Uncert Double)


fromJust' :: String -> Maybe a -> a
fromJust' s = fromMaybe (error s)


unsafeHAdd h h' = fromJust' "unsafeHAdd" $ hzip (+) h h'
unsafeHSub h h' = fromJust' "unsafeHSub" $ hzip (-) h h'
unsafeHDiv h h' = fromJust' "unsafeHDiv" $ hzip (/) h h'
unsafeHMul h h' = fromJust' "unsafeHMul" $ hzip (*) h h'

data Args =
  Args
    { mcmcfile   :: String
    , nsamples   :: Int
    , yodafolder :: String
    , xsecfile   :: String
    , observable :: String
    , infiles    :: [String]
    , test       :: String
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
  <*> strOption
    ( long "yodafolder" <> metavar "YODAFOLDER" )
  <*> strOption
    ( long "xsecfile" <> metavar "XSECFILE" )
  <*> strOption
    ( long "observable" <> metavar "OBSERVABLE" )
  <*> some (strArgument (metavar "INFILES"))
  <*> strOption
    ( long "test"
    <> help "the unfolding test to perform"
    )


main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- execParser $ info (helper <*> inArgs) fullDesc

  let (recohname', truehname, matrixhname') = obsNames $ observable args

      rename s = T.replace "elmujj" $ s <> "/elmujj"

      (recohname, matrixhname) =
        case test args of
          "mugt22" -> traceShowId (rename "mu_gt_22" recohname', rename "mu_gt_22" matrixhname')
          "mule22" -> traceShowId (rename "mu_le_22" recohname', rename "mu_le_22" matrixhname')
          _ -> (recohname', matrixhname')

      regex = intercalate "|" $ obsNames (observable args) ^.. each

  xsecs <- fromMaybe (error "failed to read xsecs") <$> readXSecFile (xsecfile args)
  procs <- either error id <$> decodeFiles (pure regex) empty (infiles args)

  let normedProcs = either error id $ itraverse (normToXsec xsecs) procs

      (data', pred', _, _) =
        either error id
        $ bfragModel (test args) normedProcs

      (bkg, migration) = unfoldingInputs (observable args) pred'

      filtVar f v =
        let nom = view nominal v
        in over variations (inSM (strictMap . HM.filter (f nom))) v

      -- only keep bkg variations with a > 5% deviation
      bkgFilt hnom hvar =
        or . view histData . fromJust' "bkgFilt"
        $ hzip' f hnom hvar
        where
          f n v =
            let d = abs (v - n) / n
            in d > 0.05

      -- only keep matrix variations with a deviation > 0.01%
      matFilt hnom hvar =
        or . view histData . fromJust' "matFilt"
        $ hzip' f hnom hvar
        where
          f n v =
            let d = abs (v - n)
            in d > 0.0001


      showMigMat :: T.Text -> H2DD -> T.Text
      showMigMat n mat =
        printScatter3D n False False
        . fmap convertBin3D
        . asList'
        $ uncertToScat <$> mat


      trueh' = fmap (view sumW) . obsTruthTrimmers (observable args)
        $ pred' ^?! ix truehname . noted . nominal . _H1DD

      -- deal with mu stress
      trueh =
        case test args of
          "mugt22" -> (*0.45) <$> trueh'
          "mule22" -> (*0.55) <$> trueh'
          _ -> trueh'
            

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


      -- TODO
      -- nsvtrksf is removed
      filtNSVSF :: Vars a -> Vars a
      filtNSVSF =
        if observable args == "nsvtrk"
          then over variations $ sans "nsvtrksf"
          else id

      (model, params') =
        buildModel
          (T.isInfixOf "statonly" . T.pack $ test args)
          (view histData trueh)
          (filtNSVSF $ getH2DD <$> filtVar matFilt (view noted migration))
          (filtNSVSF $ HM.singleton "bkg" . fmap uMean . view histData <$> filtVar bkgFilt (view noted bkg))


      -- TODO
      -- ptcsf is removed
      params =
        if observable args == "rho"
          then params' & at "ptcsf" .~ Nothing
          else params'

      pois = HM.filterWithKey (\k _ -> T.isInfixOf "truthbin" k) params

      nps = HM.filterWithKey (\k _ -> not $ T.isInfixOf "truthbin" k) params


      appParam :: Num a => Model a -> a -> ModelParam a -> Model a
      appParam m w ModelParam{..} =
        either error id
        $ appVars (Identity _mpVariation) (pure w) m


      nommodel :: Model Double
      nommodel =
        either error id
        $ appVars (_mpVariation <$> pois) (_mpInitialValue <$> pois) model


      dumpModel :: Num a => Model a -> (Vector a, Vector a, Vector (Vector a))
      dumpModel m@Model{..} =
        let pred = either error id $ prediction m
        in (_mLumi *^ foldl (^+^) zero _mBkgs, pred, _mMig)


      modelvariations :: Vars (Vector Double, Vector Double, Vector (Vector Double))
      modelvariations =
        fmap dumpModel
        . Variation nommodel
        . strictMap
        $ appParam nommodel 1 <$> nps


      writeModel
        :: T.Text
        -> Vars (Vector Double, Vector Double, Vector (Vector Double))
        -> IO ()
      writeModel t cs =
        writeFile (yodafolder args <> "/" <> T.unpack t <> ".yoda")
          . T.unpack . T.intercalate "\n\n"
          $ [ showMigMat ("/htop" <> matrixhname <> "eff") mat'
            , printScatter2D ("/htop" <> recohname) False True $ asList' reco'
            , printScatter2D ("/htop" <> recohname <> "bkg") False True $ asList' bkg'
            ] ++ showEffSlicesY matrixhname mat'
              ++ showEffSlicesX matrixhname mat'

        where
          withUncert :: (Num a, Additive v) => Vars (v a) -> v (Uncert a)
          withUncert (Variation n vs) =
            let go (x, dx2) x' = (x, dx2 ^+^ (fmap (^2) $ x ^-^ x'))
            in uncurry (liftI2 withVar) $ foldl' go (n, const 0 <$> n) vs

          bkgs = view _1 <$> cs
          reco = view _2 <$> cs
          mat = view _3 <$> cs

          withVar' :: Double -> Double -> Uncert Double
          withVar' = withVar

          matu = getCompose . withUncert $ Compose <$> mat
          bkgsu = withUncert bkgs
          recou = withUncert reco

        
          matbinning :: Bin2D (ArbBin Double) (ArbBin Double)
          matbinning = view (noted.nominal.bins) migration

          mat' :: H2DD
          mat' = ungetH2DD matbinning matu

          reco' = H.histogram recobinning $ uncertToScat <$> recou
          bkg' = H.histogram recobinning $ uncertToScat <$> bkgsu

          recobinning = view bins datah


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

  putStrLn "pois:"
  print pois
  putStrLn "nps:"
  print nps

  putStrLn "nom model:"
  print nommodel

  writeModel "total" modelvariations

  imapM_ (\t -> writeModel t . pure) $ variationToMap "nominal" modelvariations


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
        $ zipWith (\x (_, (_, (q16, q50, q84))) -> (x, (q50, (q16, q84)))) truebins unfolded'''
      hPutStrLn h . T.unpack . printScatter2D ("/REF/htop" <> truehname <> "norm") True True
        $ zipWith (\x (_, (_, (q16, q50, q84))) -> (x, (q50, (q16, q84)))) truebins unfoldednorm
      hPutStrLn h . T.unpack . printScatter2D ("/htop" <> recohname) False True
        $ zipWith (\x (_, (_, (q16, q50, q84))) -> (x, (q50, (q16, q84)))) recobins reco'

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
  let (recohname, truehname, matrixhname) = obsNames obs

      trimR
        :: Monoid b
        => Histogram Vector (ArbBin Double) b
        -> Histogram Vector (ArbBin Double) b
      trimR = obsRecoTrimmers obs

      trimT
        :: Monoid b
        => Histogram Vector (ArbBin Double) b
        -> Histogram Vector (ArbBin Double) b
      trimT = obsTruthTrimmers obs

      recoh, trueh :: Annotated (Vars H1D)
      recoh =
        fmap (trimR . (^?! _H1DD))
        <$> hs ^?! ix recohname

      trueh =
        fmap (trimT . (^?! _H1DD))
        <$> hs ^?! ix truehname

      math :: Annotated (Variation VarMap (Histogram Vector (Bin2D (ArbBin Double) (ArbBin Double)) (Dist2D Double)))
      math =
        fmap
          ( H.liftY trimR
          . H.liftX trimT
          . fromJust' "math"
          . preview _H2DD
          )
        <$> hs ^?! ix matrixhname

      recomatchh =
        over
          noted
          (fmap $ H.reduceX (hoistD (\(Pair x _) -> Only x) . H.foldl mappend mempty))
          math


      -- TODO
      -- is this properly maintaining fiducial backgrounds?
      -- what about overflows?
      bkgrecoh :: Annotated (Vars (Histogram Vector (ArbBin Double) (Dist1D Double)))
      bkgrecoh =
        liftA2 (\h h' -> fromJust' "bkgrecoh" $ removeSubHist' h h')
        <$> (fmap.fmap) rmOverflow recoh
        <*> (fmap.fmap) rmOverflow recomatchh

      -- bkgs :: Annotated (Vars H1DD)
      bkgs :: Annotated (Vars (Histogram Vector (ArbBin Double) (Uncert Double)))
      bkgs = fmap (over histVals distToUncert) <$> bkgrecoh

      normmat m h =
        H.liftX (\hm -> fromJust' "normmat" . hzip divCorr hm $ rmOverflow h) m

      mats = liftA2 normmat <$> math <*> trueh

      nommat :: H2DD
      nommat = view (noted.nominal) mats

      nombkg :: H1DD
      nombkg = view (noted.nominal) bkgs

      binningmat = focusOrCenter (\(Pair x y) -> (x, y)) id $ view (noted.nominal) math
      binningbkg = focusOrCenter (\(Only x) -> x) id $ view (noted.nominal) bkgrecoh

      smooth2D = smoothRatioUncorr2DAlongXY binningmat nommat
      smooth1D = smoothRatioUncorr1DAlongX binningbkg nombkg

      -- don't smooth for rho and nsvtrk?
      -- I think this issue is related to the over/underflow.
      -- TODO
      mats' = 
        if "zb" `T.isPrefixOf` T.pack obs
          then mats & over (noted.variations.traverse) smooth2D
          else mats

      bkgs' =
        if "zb" `T.isPrefixOf` T.pack obs
          then bkgs & over (noted.variations.traverse) smooth1D
          else bkgs

  in (bkgs', mats')


focus :: (Eq a, Fractional a, Functor f) => DistND f a -> Maybe (f a)
focus d =
  case view sumW d of
    0 -> Nothing
    x -> Just $ (/x) <$> view sumWX d


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


ungetH2DD :: (Bin bx, Bin by) => Bin2D bx by -> Vector (Vector a) -> Histogram Vector (Bin2D bx by) a
ungetH2DD b vv =
  let (nx, ny) = nBins2D b
      v = V.generate (nx*ny) (\i -> let (ix, iy) = toIndex2D b i in vv V.! ix V.! iy)
  in H.histogram b v


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


smoothRatioUncorr1DAlongX
  :: (Ord a, Floating a, BinValue b ~ a, BinEq b)
  => Histogram Vector b a
  -> Histogram Vector b (Uncert a)
  -> Histogram Vector b (Uncert a)
  -> Histogram Vector b (Uncert a)
smoothRatioUncorr1DAlongX binvals hnom hvar =
    smoothRatioUncorr1D . H.zip (,) binvals $ H.zip (,) hnom hvar


smoothRatioUncorr1D
  :: (Ord a, Floating a, BinValue bin ~ a, Bin bin)
  => Histogram Vector bin (a, (Uncert a, Uncert a))
  -> Histogram Vector bin (Uncert a)
smoothRatioUncorr1D h = H.histogramUO (view bins h) Nothing (V.fromList smoothed)
  where
    lh = views histData V.toList h

    rat = over (traverse._2) (\(nom, var) -> uMeanStd $ safeDiv var nom) lh
    (end, result) = polySmooth start 10 rat

    correction = exact . snd <$> result

    smoothed = zipWith (\(_, (nom, _)) corr -> nom*corr) lh correction

    safeDiv _ 0 = 1 +/- 1
    safeDiv 0 _ = 1 +/- 1
    safeDiv x y = x / y

    start :: Num a => [a]
    start = [1, 0, 0, 0]



smoothRatioUncorr2D
  :: (Ord a, Floating a, Bin bX, BinValue bX ~ a, Bin bY, BinValue bY ~ a)
  => Histogram Vector (Bin2D bX bY) ((a, a), (Uncert a, Uncert a))
  -> Histogram Vector (Bin2D bX bY) (Uncert a)
smoothRatioUncorr2D h = H.histogramUO (view bins h) Nothing (V.fromList smoothed)
  where
    lh = views histData V.toList h

    rat = over (traverse._2) (\(nom, var) -> uMeanStd $ safeDiv var nom) lh
    (end, result) = polySmooth2D start (2*length start) rat

    correction = exact . snd <$> result

    smoothed = zipWith (\(_, (nom, _)) corr -> nom*corr) lh correction

    safeDiv _ 0 = 1 +/- 1
    safeDiv 0 _ = 1 +/- 1
    safeDiv x y = x / y

    start :: Num a => [[a]]
    start =
      [ [1, 0, 0, 0, 0, 0]
      , [0, 0, 0, 0, 0]
      , [0, 0, 0, 0]
      , [0, 0, 0]
      , [0, 0]
      , [0]
      ]

    -- start :: Num a => [[a]]
    -- start =
    --   [ [1, 0, 0, 0, 0, 0]
    --   , [0, 0, 0, 0, 0, 0]
    --   , [0, 0, 0, 0, 0, 0]
    --   , [0, 0, 0, 0, 0, 0]
    --   , [0, 0, 0, 0, 0, 0]
    --   , [0, 0, 0, 0, 0, 0]
    --   ]


smoothRatioUncorr2DAlongXY
  :: (Ord a, Floating a, BinValue bY ~ a, BinValue bX ~ a, BinEq bX, BinEq bY)
  => Histogram Vector (Bin2D bX bY) (a, a)
  -> Histogram Vector (Bin2D bX bY) (Uncert a)
  -> Histogram Vector (Bin2D bX bY) (Uncert a)
  -> Histogram Vector (Bin2D bX bY) (Uncert a)
smoothRatioUncorr2DAlongXY binvals mnom mvar =
  smoothRatioUncorr2D . H.zip (,) binvals $ H.zip (,) mnom mvar


focusOrCenter
  :: (Eq a, Fractional a, Functor v, Bin b)
  => (v a -> c)
  -> (BinValue b -> c)
  -> Histogram Vector b (DistND v a)
  -> Histogram Vector b c
focusOrCenter f g h =
  let foci = focus <$> view histData h
      bins' = H.binsCenters $ H.bins h
  in H.histogram (H.bins h) $ V.zipWith (\bc -> maybe (g bc) f) bins' foci


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


hoistD :: Functor v => (forall x. v x -> w x) -> DistND v a -> DistND w a
hoistD f DistND{..} = DistND _sumW _sumWW (f _sumWX) (f $ f <$> _sumWXY) _nentries


instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)


rmOverflow :: Bin b => Histogram Vector b a -> Histogram Vector b a
rmOverflow = set outOfRange Nothing



instance (Additive f, Additive g) => Additive (Compose f g) where
  zero = Compose $ zero <$ (zero :: f Int)
  {-# INLINE zero #-}
  Compose a ^+^ Compose b = Compose $ liftU2 (^+^) a b
  {-# INLINE (^+^) #-}
  Compose a ^-^ Compose b = Compose $ liftU2 (^-^) a b
  {-# INLINE (^-^) #-}
  liftU2 f (Compose a) (Compose b) = Compose $ liftU2 (liftU2 f) a b
  {-# INLINE liftU2 #-}
  liftI2 f (Compose a) (Compose b) = Compose $ liftI2 (liftI2 f) a b
  {-# INLINE liftI2 #-}

