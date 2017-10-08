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
import           Control.Applicative    (liftA2, liftA3)
import           Control.Arrow          ((***))
import           Control.Lens
import           Data.HashMap.Strict    (HashMap)
import qualified Data.HashMap.Strict    as HM
import qualified Data.Histogram.Generic as H
import           Data.List              (partition, sort)
import           Data.Maybe             (fromJust, fromMaybe)
import           Data.Monoid            (Sum (..))
import           Data.Semigroup         ((<>))
import           Data.TDigest           (quantile)
import qualified Data.Text              as T
import           Data.Vector            (Vector)
import qualified Data.Vector            as V
import           GHC.Exts               (IsList (..))
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
  xsecs <- fromMaybe (error "failed to read xsecs") <$> readXSecFile xsecfile
  (procs :: StrictMap ProcessInfo (Sum Double, Folder (Vars YodaObj))) <- either error id <$> decodeFiles (Just regex) infs

  let hs = either error id $ itraverse norm procs

      norm (ProcessInfo _ DS) (_, hs') = return hs'
      norm (ProcessInfo ds _) (Sum w, hs') = do
        xsec <-
          toEither ("missing cross section for " ++ show ds)
          $ xsecs ^? ix ds . _1
        return $ fmap (scaleYO (xsec/w)) <$> hs'

      nomkey = ProcessInfo 410501 FS
      afiikey = ProcessInfo 410501 AFII
      radupkey = ProcessInfo 410511 AFII
      raddownkey = ProcessInfo 410512 AFII
      mekey = ProcessInfo 410225 AFII
      pskey = ProcessInfo 410525 AFII
      datakey = ProcessInfo 0 DS

      zjetskeys = flip ProcessInfo FS <$> [364128..364141]
      stopkeys = flip ProcessInfo FS <$> [410015, 410016]

      addVar name f vs = vs & variations . at name ?~ f (view nominal vs)

      zjets =
        fmap (addVar "ZJetsNormUp" (scaleYO 1.3))
        . fromMaybe (error "missing zjets") $ getProcs zjetskeys
      stop =
        fmap (addVar "STopNormUp" (scaleYO 1.3))
        . fromMaybe (error "missing stop") $ getProcs stopkeys
      nonttbar = zjets `mappend` stop

      nom = fromMaybe (error "missing nominal") $ getProcs [nomkey]
      afii = fromMaybe (error "missing afii") $ getProcs [afiikey]
      radup = fromMaybe (error "missing radup") $ getProcs [radupkey]
      raddown = fromMaybe (error "missing raddown") $ getProcs [raddownkey]
      me = fromMaybe (error "missing me") $ getProcs [mekey]
      ps = fromMaybe (error "missing ps") $ getProcs [pskey]


      getProcs pis = do
        hs' <- traverse (\p -> view (at p) hs) pis
        return $ mconcat hs'

      (nombkg', nommat') = toModel nonttbar nom
      (afbkg, afmat) = (view nominal *** view nominal) $ toModel nonttbar afii
      (radupbkg, radupmat) = (view nominal *** view nominal) $ toModel nonttbar radup
      (raddownbkg, raddownmat) = (view nominal *** view nominal) $ toModel nonttbar raddown
      (mebkg, memat) = (view nominal *** view nominal) $ toModel nonttbar me
      (psbkg, psmat) = (view nominal *** view nominal) $ toModel nonttbar ps

      -- vardiff: when we have a different nominal to compare (e.g. AFII)
      vardiff nom' nom'' var = unsafeHAdd nom' $ unsafeHSub var nom''

      -- vardiff2: when we compare up and down variation and symmetrize
      vardiff2 nom' varup vardown =
        unsafeHAdd nom' . fmap (/2) $ unsafeHSub varup vardown

      collapseVars (Variation n vs) =
        let vs' = toList vs
            filt s = T.isSuffixOf (T.toLower s) . T.toLower
            rm s =
              fmap $ \(x, y) ->
                let x' = T.toLower x
                    s' = T.toLower s
                in (fromMaybe x' (T.stripSuffix s' x'), y)

            (downs, ups) =
              ((HM.fromList . rm "down") *** (HM.fromList . rm "up"))
              $ partition (filt "down" . fst) vs'
        in Variation n . strictMap $ HM.unionWith (liftA3 vardiff2 n) ups downs



      bkg =
        let nom' = view nominal nombkg'
            go x = vardiff <$> nom' <*> afbkg <*> x
            nom'' = view noted nom'
        in
          nombkg'
          & variations . at "PSUp" ?~ go psbkg
          & variations . at "MEUp" ?~ go mebkg
          & variations . at "RadUp" ?~ go radupbkg
          & variations . at "RadDown" ?~ go raddownbkg
          & collapseVars
          & variations %~ filtVar (bkgFilt nom'')

      -- only keep bkg variations with a > 2% deviation
      bkgFilt hnom hvar =
        any go . view histData . fromJust
        $ hzip' f hnom hvar
        where
          f n v = (n, v - n)
          go (n, d) = abs (1 - d / n) > 0.02

      mat =
        let nom' = view nominal nommat'
            go x = vardiff <$> nom' <*> afmat <*> x
            nom'' = view noted nom'
        in
          nommat'
          & variations . at "PSUp" ?~ go psmat
          & variations . at "MEUp" ?~ go memat
          & variations . at "RadUp" ?~ go radupmat
          & variations . at "RadDown" ?~ go raddownmat
          & collapseVars
          & variations %~ filtVar (matFilt nom'')

      filtVar f = inSM (strictMap . HM.filter (f . view noted))

      -- only keep matrix variations with a deviation > 0.1% in a bin with > 0.1% efficiency
      matFilt hnom hvar =
        any go . view histData . fromJust
        $ hzip' f hnom hvar
        where
          f n v = (n, abs $ v - n)
          go (n, d) = n > 0.001 && d > 0.001

      mats = (fmap.fmap.fmap) doubToDist2D mat

      matdiffs :: Vars (Maybe (Annotated H2D))
      matdiffs =
        let tmp = Just <$> mat
            vs = tmp & nominal .~ Nothing
            n = tmp & variations .~ mempty
            diffs = (liftA2.liftA2.liftA2) unsafeHSub vs n
        in (fmap.fmap.fmap.fmap) doubToDist2D diffs

      doubToDist2D :: Double -> Dist2D Double
      doubToDist2D w = filling (Pair 0 0) w mempty

      showMigMat n ao =
        printYodaObj ("/htop" <> n)
          $ H2DD . scaleByBinSize2D <$> ao

      writeMigs t (m, mdiff) =
        withFile (youtfolder <> "/" <> T.unpack t <> ".yoda") WriteMode $ \h ->
          hPutStrLn h . T.unpack . T.intercalate "\n\n"
          $ [ showMigMat (matrixname <> "eff") m
            , maybe "" (showMigMat $ matrixname <> "diff") mdiff
            ]

      recoh =
        mappend nom nonttbar ^?! ix recohname . nominal . noted . _H1DD
      trueh =
        fmap (view sumW) . trimTrueH
        $ hs ^?! ix nomkey . ix truehname . nominal . noted . _H1DD

      datah :: H1DI
      datah =
        fmap (round . view sumW) . trimRecoH
        . fromMaybe (scaling (view nominal lumi) recoh)
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

  imapM_ writeMigs . variationToMap "nominal" $ liftA2 (,) mats matdiffs

  putStrLn "data:"
  views histData print datah

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
      hPutStrLn h . T.unpack . printScatter2D ("/REF/htop" <> truehname)
        $ zipWith (\x (_, y) -> (x, y)) xs unfolded''
      hPutStrLn h . T.unpack . printScatter2D ("/REF/htop" <> truehname <> "norm")
        $ zipWith (\x (_, y) -> (x, y)) xs unfoldednorm

  where
    scaleYO :: Double -> YodaObj -> YodaObj
    scaleYO w (Annotated as (H1DD h)) = Annotated as (H1DD $ scaling w h)
    scaleYO w (Annotated as (H2DD h)) = Annotated as (H2DD $ scaling w h)
    scaleYO _ h                       = h

    toEither s Nothing  = Left s
    toEither _ (Just x) = return x



toModel
  :: Folder (Vars YodaObj)
  -> Folder (Vars YodaObj)
  -> (Vars (Annotated H1DD), Vars (Annotated H2DD))
toModel nonsig sig =
  let recoh, trueh, recomatchh, bkgrecoh :: Vars (Annotated H1DD)
      recoh =
        fmap (fmap (view sumW) . trimRecoH . fromJust . preview _H1DD)
        <$> sig ^?! ix recohname
      trueh =
        fmap (fmap (view sumW) . trimTrueH . fromJust . preview _H1DD)
        <$> sig ^?! ix truehname
      recomatchh =
        fmap (fmap (view sumW) . trimRecoH . fromJust . preview _H1DD)
        <$> sig ^?! ix recomatchhname

      nonsigrecoh =
        fmap (fmap (view sumW) . trimRecoH . fromJust . preview _H1DD)
        <$> nonsig ^?! ix recohname

      math :: Vars (Annotated H2DD)
      math =
        fmap (fmap (view sumW) . H.liftY trimRecoH . H.liftX trimTrueH . fromJust . preview _H2DD)
        <$> sig ^?! ix matrixname

      bkgrecoh = liftA2 unsafeHSub <$> recoh <*> recomatchh
      bkgrecoh' = liftA2 unsafeHAdd <$> bkgrecoh <*> nonsigrecoh

  in (bkgrecoh', liftA2 normmat <$> math <*> trueh)

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


scaleByBinSize2D
  :: (BinValue b ~ (Weight a, Weight a), IntervalBin b, Fractional (Weight a), Weighted a)
  => Histogram Vector b a -> Histogram Vector b a
scaleByBinSize2D h =
  let intervals = views bins binsList h
      go ((xmin, ymin), (xmax, ymax)) =
        scaling ((xmax - xmin) * (ymax - ymin))
  in over histData (V.zipWith go intervals) h
