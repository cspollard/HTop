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
import           Control.Monad          (forM_)
import           Data.Foldable          (fold)
import           Data.HashMap.Strict    (HashMap)
import qualified Data.HashMap.Strict    as HM
import qualified Data.Histogram.Generic as H
import           Data.List              (nub)
import           Data.List              (intercalate, sort)
import           Data.Maybe             (catMaybes, fromJust, fromMaybe)
import           Data.Monoid            (Sum (..))
import           Data.Semigroup         ((<>))
import           Data.TDigest           (quantile)
import qualified Data.Text              as T
import           Data.Vector            (Vector)
import qualified Data.Vector            as V
import           Model
import           Options.Applicative
import           RunModel
import           System.IO              (BufferMode (..), IOMode (..), hPutStr,
                                         hPutStrLn, hSetBuffering, stdout,
                                         withFile)
import           Text.Printf


type TextMap = HashMap T.Text

type H1DI = Histogram V.Vector (ArbBin Double) Int
type H1DD = Histogram V.Vector (ArbBin Double) Double
type H2DD = Histogram V.Vector (Bin2D (ArbBin Double) (ArbBin Double)) Double

type H1D = Hist1D (ArbBin Double)
type H2D = Hist2D (ArbBin Double) (ArbBin Double)

unsafeHAdd h h' = fromJust $ hzip' (+) h h'
unsafeHSub h h' = fromJust $ hzip' (-) h h'
unsafeHDiv h h' = fromJust $ hzip' (/) h h'

data Args =
  Args
    { mcmcfile   :: String
    , nsamples   :: Int
    , yodafolder :: String
    , xsecfile   :: String
    , observable :: String
    , infiles    :: [String]
    } deriving (Show)

inArgs :: Parser Args
inArgs =
  Args
  <$> strOption
    ( long "mcmcfile" <> metavar "MCMCFILE" )
  <*> (
    option auto ( long "nsamples" <> metavar "NSAMPLES=1000000" )
    <|> pure 1000000
    )
  <*> strOption
    ( long "yodafolder" <> metavar "YODAFOLDER" )
  <*> strOption
    ( long "xsecfile" <> metavar "XSECFILE" )
  <*> strOption
    ( long "observable" <> metavar "OBSERVABLE" )
  <*> some (strArgument (metavar "INFILES"))


main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- execParser $ info (helper <*> inArgs) fullDesc

  let (recohname, truehname, _, matrixname) = obsNames (observable args)
      regex = intercalate "|" $ obsNames (observable args) ^.. each

  xsecs <- fromMaybe (error "failed to read xsecs") <$> readXSecFile (xsecfile args)
  procs <- either error id <$> decodeFiles (Just regex) (infiles args)

  let normedProcs = either error id $ itraverse (normToXsec xsecs) procs
      (data', pred', _, _) = either error id $ bfragModel normedProcs
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

      -- only keep matrix variations with a deviation > 0.1%
      matFilt hnom hvar =
        or . view histData . fromJust
        $ hzip' f hnom hvar
        where
          f n v =
            let d = abs (v - n)
            in d > 0.001

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
        writeFile (yodafolder args <> "/" <> T.unpack t <> ".yoda")
          . T.unpack . T.intercalate "\n\n"
          $ [ showMigMat (matrixname <> "eff") m
            , maybe "" (showMigMat $ matrixname <> "diff") mdiff
            , maybe "" (showMigMat $ matrixname <> "reldiff") mreldiff
            ]

      trueh = fmap (view sumW) . obsTrimmers (observable args)
        $ pred' ^?! ix truehname . noted . nominal . _H1DD

      datah :: H1DI
      datah =
        fmap (round . view sumW) . obsTrimmers (observable args)
        $ data' ^?! ix recohname . noted . _H1DD

      xs :: [(Double, (Double, Double))]
      xs =
        V.toList . fmap (\(mn, mx) -> ((mn+mx)/2, (mn, mx))) . binsList
        $ view bins trueh

      (model, params) =
        buildModel
          (view histData trueh)
          (getH2DD <$> filtVar matFilt (view noted migration))
          (HM.singleton "bkg" . view histData <$> filtVar bkgFilt (view noted bkg))

  imapM_ writeMigs . variationToMap "nominal"
    $ liftA3 (,,)
      (sequence migration')
      (sequence <$> sequence matdiffs)
      (sequence <$> sequence matreldiffs)

  putStrLn "data:"
  views histData print datah

  putStrLn "model:"
  print model

  (unfolded', unfoldedcov) <-
    runModel (nsamples args) (mcmcfile args) (view histData datah) model params

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

  withFile (yodafolder args <> "/htop.yoda") WriteMode $ \h -> do
      hPutStrLn h . T.unpack . printScatter2D ("/REF/htop" <> truehname)
        $ zipWith (\x (_, y) -> (x, y)) xs unfolded''
      hPutStrLn h . T.unpack . printScatter2D ("/REF/htop" <> truehname <> "norm")
        $ zipWith (\x (_, y) -> (x, y)) xs unfoldednorm

  let reluncerts =
        flip HM.mapMaybeWithKey unfoldedcov $ \(name, name') cov ->
          let val =
                fromMaybe (error "missing best fit value") $ do
                  (mx, _) <- HM.lookup name unfolded'
                  mx

              var' =
                fromMaybe (error "missing variance")
                $ HM.lookup (name', name') unfoldedcov

          in
            if T.isPrefixOf "normtruthbin" name
                && not (T.isPrefixOf "truthbin" name')
                && not (T.isPrefixOf "recobin" name')
                && name' /= "llh"
              then Just . abs $ cov / var' / val
              else Nothing


      latex =
        let ks = HM.keys reluncerts
            poinames = sort . nub $ fst <$> ks
            names = sort . nub $ snd <$> ks
            hmlookup k hm = fromMaybe (error "can't find key") $ HM.lookup k hm
            fmtLine poiname =
              T.unpack poiname
              ++ " & "
              ++ intercalate " & "
                  ( printf "%.2f" . flip hmlookup reluncerts . (poiname,)
                    <$> names
                  )
              ++ "\\\\"

        in unlines $
          [ "\\begin{tabular}{ l " ++ fold (replicate (length names) "| c ") ++ "}"
          , " & " ++ intercalate " & " (T.unpack <$> names) ++ " \\\\"
          ]
          ++ (fmtLine <$> poinames)
          ++ ["\\end{tabular}"]


  withFile (yodafolder args <> "/htop.stat") WriteMode $ \h -> do
    print "covariances:"
    print unfoldedcov

    hPutStrLn h latex

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

      trim
        :: (Fractional a, Ord a, Monoid b)
        => Histogram Vector (ArbBin a) b -> Histogram Vector (ArbBin a) b
      trim = obsTrimmers obs

      recoh, trueh, recomatchh, bkgrecoh :: Annotated (Vars H1DD)
      recoh =
        fmap (fmap (view sumW) . trim . (^?! _H1DD))
        <$> hs ^?! ix recohname
      trueh =
        fmap (fmap (view sumW) . trim . (^?! _H1DD))
        <$> hs ^?! ix truehname
      recomatchh =
        fmap (fmap (view sumW) . trim . (^?! _H1DD))
        <$> hs ^?! ix recomatchhname

      math :: Annotated (Vars H2DD)
      math =
        fmap (fmap (view sumW) . H.liftY trim . H.liftX trim . fromJust . preview _H2DD)
        <$> hs ^?! ix matrixname

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
        , ModelParam x NonNegative
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
