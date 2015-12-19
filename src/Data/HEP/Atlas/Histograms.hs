{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Data.HEP.Atlas.Histograms where

import Control.Applicative ((<$>))
import Control.Arrow
import Data.Monoid

import Data.Text (Text)
import qualified Data.Text as T

import Data.Maybe (isJust, fromJust, listToMaybe)

import Data.Histogram.Generic (Histogram(..), underflows, overflows)
import qualified Data.Histogram.Generic as HG
import Data.Histogram.Fill

import Data.Traversable (sequenceA)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G

import Data.HEP.Atlas.Event
import Data.HEP.LorentzVector

import qualified Data.Map as M


ptBins :: BinD
ptBins = binD 0 50 500

eBins :: BinD
eBins = binD 0 50 500

mBins :: BinD
mBins = binD 0 50 200

etaBins :: BinD
etaBins = binD (-3) 50 3

phiBins :: BinD
phiBins = binD (-pi) 50 pi


-- a YodaHist is just a histogram with some annotations.
data YodaHist v b val = YodaHist {
                    yhAnnots :: M.Map Text Text, 
                    yhHist :: Histogram v b val
                    }

alterAnnots :: (M.Map Text Text -> M.Map Text Text) -> YodaHist v b val -> YodaHist v b val
alterAnnots f (YodaHist yha yhh) = YodaHist (f yha) yhh

alterHist :: (Histogram v b val -> Histogram v b val) -> YodaHist v b val -> YodaHist v b val
alterHist f (YodaHist yha yhh) = YodaHist yha $ f yhh

type YodaHistD = YodaHist V.Vector BinD (BinData Double)

-- strict in args to prevent histograms from taking up infinite space
data BinData a = BinData {
                    sumw :: !(Sum a),
                    sumw2 :: !(Sum a),
                    sumwx :: !(Sum a),
                    sumwx2 :: !(Sum a),
                    numEntries :: !(Sum Int)
                    } deriving (Show, Eq, Ord)


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



-- probably could be generalized.
hist :: (Bin b, G.Vector v (BinData (BinValue b)), Num (BinValue b)) =>
            b -> HBuilder (BinValue b, BinValue b) (Histogram v b (BinData (BinValue b)))
hist binning = mkFoldBuilderG binning mempty (\v xw -> v <> toBinData xw)
            <<- \(x, w) -> (x, (x, w))


yodaHist :: [(Text, Text)] -> BinD -> HBuilder (Double, Double) YodaHistD
yodaHist annots binning = YodaHist (M.fromList annots) <$> hist binning


-- only use the first (if any) of a list for filling
fillFirst :: HBuilder (a, val) b -> HBuilder ([a], val) b
fillFirst h = h <<- first fromJust <<? (isJust . fst) <<- first listToMaybe


-- fill all instances with the same value
fillAll :: HBuilder (a, val) b -> HBuilder ([a], val) b
fillAll h = h <<-| \(as, v) -> zip as (repeat v)


-- suite of histograms for LorentzVectors
lvHists :: HasLorentzVector a => Text -> Text -> HBuilder (a, Double) [YodaHistD]
lvHists path xtitle = sequenceA [
                      yodaHist [("Path", path <> "pt"), ("XLabel", xtitle <> "$p_{\\mathrm T}$ [GeV]")] ptBins <<- first ((/ 1e3) . lvPt),
                      yodaHist [("Path", path <> "E"), ("XLabel", xtitle <> "$E$ [GeV]")] eBins <<- first ((/ 1e3) . lvE),
                      yodaHist [("Path", path <> "mass"), ("XLabel", xtitle <> "mass [GeV]$")] mBins <<- first ((/ 1e3) . lvM),
                      yodaHist [("Path", path <> "eta"), ("XLabel", xtitle <> "$\\eta$")] etaBins <<- first lvEta,
                      yodaHist [("Path", path <> "phi"), ("XLabel", xtitle <> "$\\phi$")] phiBins <<- first lvPhi
                     ] <<- first toPtEtaPhiE



eventHists :: Text -> HBuilder Event [YodaHistD]
eventHists syst = concat <$> sequenceA [
                          fillAll (lvHists (syst <> "/jets/") "small-$R$ jet ") <<- first eJets,
                          fillAll (lvHists (syst <> "/largejets/") "large-$R$ jet ") <<- first eLargeJets,
                          fillAll (lvHists (syst <> "/electrons/") "electron ") <<- first eElectrons,
                          fillAll (lvHists (syst <> "/muons/") "muon ") <<- first eMuons,
                          fillFirst (lvHists (syst <> "/jet0/") "leading small-$R$ jet ") <<- first eJets,
                          fillFirst (lvHists (syst <> "/largejet0/") "leading large-$R$ jet ") <<- first eLargeJets,
                          fillFirst (lvHists (syst <> "/electron0/") "leading electron ") <<- first eElectrons,
                          fillFirst (lvHists (syst <> "/muon0/") "leading muon ") <<- first eMuons,
                          lvHists (syst <> "/met/") "$E/{\\mathrm T}^{\\mathrm miss} " <<- first eMET
                        ] <<- (id &&& weight syst)

eventSystHists :: [Text] -> HBuilder Event [YodaHistD]
eventSystHists systs = concat <$> traverse eventHists systs

integral :: (G.Vector v val, Bin b, Monoid val) => Histogram v b val -> val
integral h = HG.foldl (<>) mempty h <> fromJust (underflows h) <> fromJust (overflows h)

toTuple :: (G.Vector v val, G.Vector v ((BinValue b, BinValue b), val), G.Vector v (BinValue b, BinValue b), IntervalBin b) =>
            Histogram v b val -> [((BinValue b, BinValue b), val)]
toTuple h = G.toList $ G.zip (HG.binsList . HG.bins $ h) (HG.histData h)


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
