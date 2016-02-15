{-# LANGUAGE OverloadedStrings, FlexibleContexts, DeriveGeneric #-}

module Data.HEP.Atlas.Histograms where

import Control.Applicative ((<$>))
import Control.Arrow
import Data.Monoid

import Data.Text (Text)
import qualified Data.Text as T

import Data.Maybe (isJust, fromJust, listToMaybe)

import Data.Histogram
import Data.Binary (Binary(..))
import GHC.Generics (Generic)

import Data.Traversable (sequenceA)

import Data.HEP.Atlas.Event
import Data.HEP.LorentzVector

import qualified Data.Map as M


type Histo1D = Histogram (BinData Double) Double

ptHist :: Histo1D
ptHist = histogram 50 (0, 500) mempty

eHist :: Histo1D
eHist = histogram 50 (0 500) mempty

mHist :: Histo1D
mHist = histogram 50 (0 200) mempty

etaHist :: Histo1D
etaHist = histogram 50 (-3, 3) mempty

phiHist :: Histo1D
phiHist = histogram 50 (-pi, pi) mempty


-- a YodaHist is just a histogram with some annotations.
data YodaHist val b = YodaHist {
                    yhAnnots :: M.Map Text Text, 
                    yhHist :: Histogram val b
                    } deriving Generic

instance (Binary val, Binary b) => Binary (YodaHist val b) where

alterAnnots :: (M.Map Text Text -> M.Map Text Text) -> YodaHist val b -> YodaHist val b
alterAnnots f (YodaHist yha yhh) = YodaHist (f yha) yhh

alterHist :: (Histogram val b -> Histogram val b) -> YodaHist val b -> YodaHist val b
alterHist f (YodaHist yha yhh) = YodaHist yha $ f yhh

type YodaHistD = YodaHist (BinData Double) Double

-- strict in args to prevent histograms from taking up infinite space
data BinData a = BinData {
                    sumw :: !(Sum a),
                    sumw2 :: !(Sum a),
                    sumwx :: !(Sum a),
                    sumwx2 :: !(Sum a),
                    numEntries :: !(Sum Int)
                    } deriving (Show, Eq, Ord, Generic)


instance (Binary a) => Binary (Sum a) where
    put = put . getSum
    get = Sum <$> get

instance (Binary a) => Binary (BinData a) where


-- make BinData out of a (val, weight) tuple
toBinData :: Num a => (a, a) -> BinData a
toBinData (x, w) = BinData (Sum w) (Sum (w*w)) (Sum (w*x)) (Sum (w*w*x*x)) (Sum 1)


instance Num a => Monoid (BinData a) where
    mempty = BinData mempty mempty mempty mempty mempty
    h `mappend` h' = BinData
                    (sumw h <> sumw h')
                    (sumw2 h <> sumw2 h')
                    (sumwx h <> sumwx h')
                    (sumwx2 h <> sumwx2 h')
                    (numEntries h <> numEntries h')



yodaHistBuilder :: [(Text, Text)] -> Histo1D -> Builder (Double, Double) YodaHistD
yodaHistBuilder annots hist = premap (fst &&& toBinData) $ YodaHist (M.fromList annots) <$> histBuilder (<>) hist

test :: Builder (Double, Double) [YodaHistD]
test = sequenceA [ yodaHistBuilder [] ptHist
                 , yodaHistBuilder [] eHist 
                 ] <<- first (*2)

-- suite of histograms for LorentzVectors
-- TODO
-- instance Num b => Num (a -> b) where
lvHists :: HasLorentzVector a => Text -> Text -> Builder (a, Double) [YodaHistD]
lvHists path xtitle = sequenceA [
                      yodaHistBuilder [("Path", path <> "pt"), ("XLabel", xtitle <> "$p_{\\mathrm T}$ [GeV]")] ptHist <<- first ((/ 1e3) . lvPt)
                    , yodaHistBuilder [("Path", path <> "E"), ("XLabel", xtitle <> "$E$ [GeV]")] eHist <<- first ((/ 1e3) . lvE)
                    , yodaHistBuilder [("Path", path <> "mass"), ("XLabel", xtitle <> "mass [GeV]$")] mHist <<- first ((/ 1e3) . lvM)
                    , yodaHistBuilder [("Path", path <> "eta"), ("XLabel", xtitle <> "$\\eta$")] etaHist <<- first lvEta
                    , yodaHistBuilder [("Path", path <> "phi"), ("XLabel", xtitle <> "$\\phi$")] phiHist <<- first lvPhi
                    ] <<- first toPtEtaPhiE



-- TODO Need a way to take Builder a b -> Builder [a] b
eventHists :: Text -> Builder Event [[YodaHistD]]
eventHists syst = sequenceA [
                  foldrBuilder (lvHists (syst <> "/jets/") "small-$R$ jet ") <<- first eJets
                , foldrBuilder (lvHists (syst <> "/largejets/") "large-$R$ jet ") <<- first eLargeJets
                , foldrBuilder (lvHists (syst <> "/electrons/") "electron ") <<- first eElectrons
                , foldrBuilder (lvHists (syst <> "/muons/") "muon ") <<- first eMuons
                , foldrBuilder (lvHists (syst <> "/jet0/") "leading small-$R$ jet ") <<- first (listToMaybe . eJets)
                , foldrBuilder (lvHists (syst <> "/largejet0/") "leading large-$R$ jet ") <<- first (listToMaybe . eLargeJets)
                , foldrBuilder (lvHists (syst <> "/electron0/") "leading electron ") <<- first (listToMaybe . eElectrons)
                , foldrBuilder (lvHists (syst <> "/muon0/") "leading muon ") <<- first (listToMaybe . eMuons)
                , lvHists (syst <> "/met/") "$E/{\\mathrm T}^{\\mathrm miss} " <<- first eMET
                ] <<- (id &&& weight syst)

eventSystHists :: [Text] -> Event -> [[[YodaHistD]]] -> [[[YodaHistD]]]
eventSystHists = traverse eventHists


{-
showHist :: Text -> YodaHistD -> Text
showHist path (YodaHist annots h) = T.unlines $
                            [ "# BEGIN YODA_HISTO1D " <> path', "Path=" <> path', "Type=Histo1D" ] ++
                            -- write annotations
                            map (\(t, a) -> t <> "=" <> a) (M.toList annots) ++
                            [
                            -- fromJust is dangerous here. there
                            -- should be some default behavior.
                            "Total\tTotal\t" <> binDataToText (integral h),
                            "Underflow\tUnderflow\t" <> binDataToText (fromJust $ underflows h),
                            "Overflow\tOverflow\t" <> binDataToText (fromJust $ overflows h)
                            ] ++
                            map (\((xmin, xmax), b) -> T.pack (show xmin ++ "\t" ++ show xmax ++ "\t") <> binDataToText b) (toTuple h) ++
                            [ "# END YODA_HISTO1D", "" ]

                            where
                                path' = path <> (M.!) annots "Path"
                                binDataToText b = T.pack $
                                                show (getSum $ sumw b) ++ "\t" ++
                                                show (getSum $ sumw2 b) ++ "\t" ++
                                                show (getSum $ sumwx b) ++ "\t" ++
                                                show (getSum $ sumwx2 b) ++ "\t" ++
                                                show (getSum $ numEntries b)
                                                -}
