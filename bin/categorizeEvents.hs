{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as BSL

import Data.Maybe (fromJust)

import Data.HEP.Atlas.TopTree
import Data.HEP.Atlas.Event
import Data.HEP.Atlas.Jet
import Data.HEP.Cut
import Data.HEP.LorentzVector

import Data.Histogram.Generic hiding (zip, map, sum)
import qualified Data.Histogram.Generic as H
import Data.Histogram.Fill

import Control.Monad (forM_, liftM)
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Monoid

import qualified Data.Vector as V
import Data.Uncertain

import Data.Hep.Atlas.Histograms 

minPt :: HasLorentzVector a => Double -> Cut a
minPt x = (> x) . lvPt . toPtEtaPhiE

maxAbsEta :: HasLorentzVector a => Double -> Cut a
maxAbsEta x = (< x) . abs . lvEta . toPtEtaPhiE

minMV2c20 :: Double -> Cut Jet
minMV2c20 x = (> x) . jMV2c20

nBtags :: Event -> Int
nBtags = nJets $ minPt 25000 `cAnd` maxAbsEta 2.5 `cAnd` minMV2c20 0.7


showHist :: Text -> Text -> Text -> Histogram V.Vector BinD (BinData Double) -> Text
showHist path title xlabel h = T.unlines $
                            [
                            "# BEGIN YODA_HISTO1D " <> path,
                            "Path=" <> path,
                            "Type=Histo1D",
                            "Title=" <> title,
                            "XLabel=" <> xlabel,
                            -- fromJust is dangerous here. there
                            -- should be some default behavior.
                            pack $ "Total\tTotal\t" ++ binDataToStr (H.foldl (<>) mempty h <> fromJust (underflows h) <> fromJust (overflows h)),
                            pack $ "Underflow\tUnderflow\t" ++ binDataToStr (fromJust $ underflows h),
                            pack $ "Overflow\tOverflow\t" ++ binDataToStr (fromJust $ overflows h)
                            ] ++
                            map (\((xmin, xmax), b) -> pack $ show xmin ++ "\t" ++ show xmax ++ "\t" ++ binDataToStr b) (toTuple h)
                            ++ [
                            "# END YODA_HISTO1D",
                            ""
                            ]

                             where
                                 binDataToStr b = show (getSum $ sumw b) ++ "\t" ++
                                                show (getSum $ sumw2 b) ++ "\t" ++
                                                show (getSum $ sumwx b) ++ "\t" ++
                                                show (getSum $ sumwx2 b) ++ "\t" ++
                                                show (getSum $ numEntries b)


toTuple :: IntervalBin b => Histogram V.Vector b a -> [((BinValue b, BinValue b), a)]
toTuple h = V.toList $ V.zip (binsList . bins $ h) (histData h)

main :: IO ()
main = do
        evts <- liftM parseTree BSL.getContents :: IO Events

        let hists = fillBuilder allHists evts

        forM_ hists $ \(t, hists') -> do
            forM_ hists' $ \(s, h) -> do
                putStr . T.unpack $ (showHist ("/HTop/" <> t <> s) t s h)
