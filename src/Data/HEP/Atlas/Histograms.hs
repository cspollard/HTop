{-# LANGUAGE OverloadedStrings, FlexibleContexts, DeriveGeneric #-}

module Data.HEP.Atlas.Histograms where

import Control.Applicative ((<$>))
import Control.Arrow
import Data.Monoid

import Data.Text (Text)
import qualified Data.Text as T

import Data.Maybe (listToMaybe)

import Data.Histogram
import Data.Builder
import Data.Histogram.Bin
import Data.Binary (Binary(..))
import GHC.Generics (Generic)

import Data.Traversable (sequenceA)

import Data.Maybe (fromJust)

import Data.HEP.Atlas.Event
import Data.HEP.LorentzVector

import qualified Data.Map as M


type Histo1D = Histogram (Bin1D Double) (BinData Double)

ptHist :: Histo1D
ptHist = histogram (Bin1D 50 (0, 500)) mempty

eHist :: Histo1D
eHist = histogram (Bin1D 50 (0, 500)) mempty

mHist :: Histo1D
mHist = histogram (Bin1D 50 (0, 200)) mempty

etaHist :: Histo1D
etaHist = histogram (Bin1D 50 (-3, 3)) mempty

phiHist :: Histo1D
phiHist = histogram (Bin1D 50 (-pi, pi)) mempty


-- a YodaHist is just a histogram with some annotations.
data YodaHist b val = YodaHist {
                    yhAnnots :: M.Map Text Text, 
                    yhHist :: !(Histogram b val)
                    } deriving (Generic, Show)

instance (Binary val, Binary b) => Binary (YodaHist b val) where

alterAnnots :: (M.Map Text Text -> M.Map Text Text) -> YodaHist b val -> YodaHist b val
alterAnnots f (YodaHist yha yhh) = YodaHist (f yha) yhh

alterHist :: (Histogram b val -> Histogram b val') -> YodaHist b val -> YodaHist b val'
alterHist f (YodaHist yha yhh) = YodaHist yha $ f yhh

type YodaHistD = YodaHist (Bin1D Double) (BinData Double)

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


scaleW :: Num a => BinData a -> a -> BinData a
scaleW (BinData (Sum w) (Sum w2) (Sum wx) (Sum wx2) n) k =
            BinData (Sum $ w*k) (Sum $ w2*k*k) (Sum $ wx*k) (Sum $ wx2*k) n

-- make BinData out of a (val, weight) tuple
toBinData :: Num a => (a, a) -> BinData a
toBinData (x, w) = BinData (Sum w) (Sum (w*w)) (Sum (w*x)) (Sum (w*x*x)) (Sum 1)

haddUnsafe :: YodaHistD -> YodaHistD -> YodaHistD
haddUnsafe (YodaHist an h) (YodaHist an' h') = YodaHist an' (fromJust $ h `hadd` h')

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


fillFirst :: Builder (a, b) c -> Builder ([a], b) c
fillFirst b = foldBuilder b <<- \(xs, w) ->
                                    case listToMaybe xs of
                                        Just x -> Just (x, w)
                                        Nothing -> Nothing


fillAll :: Builder (a, b) c -> Builder ([a], b) c
fillAll b = foldBuilder b <<- (uncurry zip . (id *** repeat))


-- suite of histograms for LorentzVectors
-- TODO
-- instance Num b => Num (a -> b) where
lvHists :: HasLorentzVector a => Text -> Text -> Builder (a, Double) [YodaHistD]
lvHists path xtitle = sequenceA [
                      yodaHistBuilder [("Path", path <> "pt"), ("XLabel", xtitle <> "$p_{\\mathrm T}$ [GeV]")] ptHist <<- first ((/ 1e3) . lvPt)
                    , yodaHistBuilder [("Path", path <> "E"), ("XLabel", xtitle <> "$E$ [GeV]")] eHist <<- first ((/ 1e3) . lvE)
                    , yodaHistBuilder [("Path", path <> "mass"), ("XLabel", xtitle <> "mass [GeV]")] mHist <<- first ((/ 1e3) . lvM)
                    , yodaHistBuilder [("Path", path <> "eta"), ("XLabel", xtitle <> "$\\eta$")] etaHist <<- first lvEta
                    , yodaHistBuilder [("Path", path <> "phi"), ("XLabel", xtitle <> "$\\phi$")] phiHist <<- first lvPhi
                    ] <<- first toPtEtaPhiE



-- TODO
-- we call, e.g. eJets twice per event.
-- could be cleaned up.
eventHists :: Text -> Builder Event [[YodaHistD]]
eventHists syst = sequenceA [
                  fillAll (lvHists (syst <> "/jets/") "small-$R$ jet ") <<- first eJets
                , fillAll (lvHists (syst <> "/largejets/") "large-$R$ jet ") <<- first eLargeJets
                , fillAll (lvHists (syst <> "/trackjets/") "track jet ") <<- first eTrackJets
                , fillAll (lvHists (syst <> "/electrons/") "electron ") <<- first eElectrons
                , fillAll (lvHists (syst <> "/muons/") "muon ") <<- first eMuons
                , fillFirst (lvHists (syst <> "/jet0/") "leading small-$R$ jet ") <<- first eJets
                , fillFirst (lvHists (syst <> "/largejet0/") "leading large-$R$ jet ") <<- first eLargeJets
                , fillAll (lvHists (syst <> "/trackjet0/") "leading track jet ") <<- first eTrackJets
                , fillFirst (lvHists (syst <> "/electron0/") "leading electron ") <<- first eElectrons
                , fillFirst (lvHists (syst <> "/muon0/") "leading muon ") <<- first eMuons
                , lvHists (syst <> "/met/") "$E_{\\mathrm T}^{\\mathrm miss} " <<- first eMET
                ] <<- (id &&& weight syst)

eventSystHists :: [Text] -> Builder Event [[[YodaHistD]]]
eventSystHists = traverse eventHists


showHist :: Text -> YodaHistD -> Text
showHist path (YodaHist annots h) = T.unlines $
                            [ "# BEGIN YODA_HISTO1D " <> path', "Path=" <> path', "Type=Histo1D" ] ++
                            -- write annotations
                            map (\(t, a) -> t <> "=" <> a) (M.toList $ M.delete "Path" annots) ++
                            [
                            "Total\tTotal\t" <> binDataToText (integral h),
                            "Underflow\tUnderflow\t" <> binDataToText (underflow h),
                            "Overflow\tOverflow\t" <> binDataToText (overflow h)
                            ] ++
                            map (\((xmin, xmax), b) -> T.pack (show xmin ++ "\t" ++ show xmax ++ "\t") <> binDataToText b) (toTuples h) ++
                            [ "# END YODA_HISTO1D", "" ]

                            where
                                path' = path <> (M.!) annots "Path"
                                binDataToText b = T.pack $
                                                show (getSum $ sumw b) ++ "\t" ++
                                                show (getSum $ sumw2 b) ++ "\t" ++
                                                show (getSum $ sumwx b) ++ "\t" ++
                                                show (getSum $ sumwx2 b) ++ "\t" ++
                                                show (getSum $ numEntries b)
