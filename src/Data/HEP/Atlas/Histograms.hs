{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Data.HEP.Atlas.Histograms where

import Control.Applicative ((<$>))
import Control.Arrow
import Data.Monoid

import Data.Text (Text)
import Data.Maybe (isJust, fromJust, listToMaybe)

import Data.Histogram.Generic hiding (zip)
import Data.Histogram.Fill

import Data.Traversable (sequenceA)
import qualified Data.Vector as V

import Data.HEP.Atlas.Event
import Data.HEP.LorentzVector



ptBins :: BinD
ptBins = binD 0 50 500e3

eBins :: BinD
eBins = binD 0 50 500e3

mBins :: BinD
mBins = binD 0 50 200e3

etaBins :: BinD
etaBins = binD (-3) 50 3

phiBins :: BinD
phiBins = binD (-pi) 50 pi


type Named a = (Text, a)

data BinData a = BinData {
                    sumw :: !(Sum a),
                    sumw2 :: !(Sum a),
                    sumwx :: !(Sum a),
                    sumwx2 :: !(Sum a),
                    numEntries :: !(Sum Int)
                    } deriving (Show, Eq, Ord)


-- make an entry out of a tuple
toBinDataEntry :: Num a => (a, a) -> BinData a
toBinDataEntry (x, w) = BinData (Sum w) (Sum (w*w)) (Sum (w*x)) (Sum (w*w*x*x)) (Sum 1)


instance Num a => Monoid (BinData a) where
    mempty = BinData mempty mempty mempty mempty mempty
    h `mappend` h' = BinData
                    (sumw h <> sumw h')
                    (sumw2 h <> sumw2 h')
                    (sumwx h <> sumwx h')
                    (sumwx2 h <> sumwx2 h')
                    (numEntries h <> numEntries h')


-- a histogram for an observable from type 'a' with a name.
-- this could be generalized away from BinD and Doubles.
obsHist :: Text -> BinD -> (a -> Double) ->
            HBuilder (a, Double) (Named (Histogram V.Vector BinD (BinData Double)))
obsHist name binning obs = (name, ) <$>
            mkFoldBuilderG binning mempty f
            <<- \(a, w) -> let t = obs a in (t, (t, w))
    where
        f :: BinData Double -> (Double, Double) -> BinData Double
        f contents (x, w) = contents <> toBinDataEntry (x, w)


-- suite of histograms for LorentzVectors
lvHists :: (HasLorentzVector a) =>
            HBuilder (a, Double) [Named (Histogram V.Vector BinD (BinData Double))]
lvHists = sequenceA [
                      obsHist "pt" ptBins lvPt,
                      obsHist "E" eBins lvE,
                      obsHist "m" mBins lvM,
                      obsHist "eta" etaBins lvEta,
                      obsHist "phi" phiBins lvPhi
                     ] <<- first toPtEtaPhiE


-- only use the first (if any) of a list for filling
firstHist :: HBuilder (a, val) b -> HBuilder ([a], val) b
firstHist h = h <<- first fromJust <<? (isJust . fst) <<- first listToMaybe


-- fill all instances with the same value
allHist :: HBuilder (a, val) b -> HBuilder ([a], val) b
allHist h = h <<-| \(as, v) -> zip as (repeat v)


allHists :: HBuilder Event [Named [Named (Histogram V.Vector BinD (BinData Double))]]
allHists = sequenceA [
                          ("Jets", ) <$> allHist lvHists <<- first eJets,
                          ("LargeJets", ) <$> allHist lvHists <<- first eLargeJets,
                          ("Electrons", ) <$> allHist lvHists <<- first eElectrons,
                          ("Muons", ) <$> allHist lvHists <<- first eMuons,
                          ("LeadJet", ) <$> firstHist lvHists <<- first eJets,
                          ("LeadLargeJet", ) <$> firstHist lvHists <<- first eLargeJets,
                          ("LeadElectron", ) <$> firstHist lvHists <<- first eElectrons,
                          ("LeadMuon", ) <$> firstHist lvHists <<- first eMuons
                        ] <<- (id &&& weight)
