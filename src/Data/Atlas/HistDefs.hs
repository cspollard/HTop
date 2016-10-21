{-# LANGUAGE OverloadedStrings #-}

module Data.Atlas.HistDefs where

import Control.Lens
import Data.Semigroup

import Data.Text (Text)
import Data.YODA.Obj

dsigdXpbY :: Text -> Text -> Text
dsigdXpbY x y = "$\\frac{d\\sigma}{d" <> x <> "} \\frac{\\mathrm{pb}}{" <> y <> "}$"

gev, rad, pt :: Text
gev = "\\mathrm{GeV}"
rad = "\\mathrm{rad}"
pt = "p_{\\mathrm{T}}"


yodaHist :: Int -> Double -> Double -> Text -> Text -> Text -> YodaObj
yodaHist nb xmin xmax p xl yl = yodaHist1D nb xmin xmax
    & annots . at "Path"   ?~ p
    & annots . at "XLabel" ?~ xl
    & annots . at "YLabel" ?~ yl

yodaProf :: Int -> Double -> Double -> Text -> Text -> Text -> YodaObj
yodaProf nb xmin xmax p xl yl = yodaProf1D nb xmin xmax
    & annots . at "Path"   ?~ p
    & annots . at "XLabel" ?~ xl
    & annots . at "YLabel" ?~ yl

-- common histograms for LorentzVectors

muHist :: YodaObj
muHist = yodaHist 25 0 100 "/mu" "$< \\mu >$" $ dsigdXpbY "\\mu" "1"

ptHist :: YodaObj
ptHist = yodaHist 25 0 500 "/pt" "$p_{\\mathrm T}$ [GeV]" $ dsigdXpbY pt gev

etaHist :: YodaObj
etaHist = yodaHist 30 (-3) 3 "/eta" "$\\eta$" $ dsigdXpbY "\\eta" rad

phiHist :: YodaObj
phiHist = yodaHist 30 (-pi) pi "/phi" "$\\phi$" $ dsigdXpbY "\\phi" rad

eHist :: YodaObj
eHist = yodaHist 25 0 500 "/E" "$E$ [GeV]" $ dsigdXpbY "E" gev

mHist :: YodaObj
mHist = yodaHist 30 0 300 "/mass" "mass [GeV]" $ dsigdXpbY "m" gev

mv2c10Hist :: YodaObj
mv2c10Hist = yodaHist 25 (-1) 1 "/mv2c10" "MV2c10" $ dsigdXpbY "\\mathrm{MV2c10}" "1"

trkSumPtHist :: YodaObj
trkSumPtHist = yodaHist 25 0 500 "/trksumpt" "$p_{\\mathrm T} \\sum \\mathrm{trk}$" $ dsigdXpbY pt gev

svTrkSumPtHist :: YodaObj
svTrkSumPtHist = yodaHist 25 0 500 "/svtrksumpt" "$p_{\\mathrm T} \\sum \\mathrm{SV trk}$" $ dsigdXpbY pt gev

bFragHist :: YodaObj
bFragHist = yodaHist 22 0 1.1 "/bfrag" "$z_{p_{\\mathrm T}}$" $ dsigdXpbY "z_{p_{\\mathrm T}}" "1" 

trkSumPtVsJetPtProf :: YodaObj
trkSumPtVsJetPtProf = yodaProf 18 25 250 "/trksumptvsjetptprof" "$p_{\\mathrm T}$ [GeV]" "$<p_{\\mathrm T} \\sum \\mathrm{trk}>$"

svTrkSumPtVsJetPtProf :: YodaObj
svTrkSumPtVsJetPtProf = yodaProf 18 25 250 "/svtrksumptvsjetptprof" "$p_{\\mathrm T}$ [GeV]" "$<p_{\\mathrm T} \\sum \\mathrm{SV trk}>$"

bFragVsJetPtProf :: YodaObj
bFragVsJetPtProf = yodaProf 18 25 250 "/bfragvsjetptprof" "$p_{\\mathrm T}$ [GeV]" "$<z_{p_{\\mathrm T}}>$"

trkSumPtVsJetEtaProf :: YodaObj
trkSumPtVsJetEtaProf = yodaProf 21 0 2.1 "/trksumptvsjetetaprof" "$\\eta$" "$<p_{\\mathrm T} \\sum \\mathrm{trk}>$"

svTrkSumPtVsJetEtaProf :: YodaObj
svTrkSumPtVsJetEtaProf = yodaProf 21 0 2.1 "/svtrksumptvsjetetaprof" "$\\eta$" "$<p_{\\mathrm T} \\sum \\mathrm{SV trk}>$"

bFragVsJetEtaProf :: YodaObj
bFragVsJetEtaProf = yodaProf 21 0 2.1 "/bfragvsjetetaprof" "$\\eta$" "$<z_{p_{\\mathrm T}}>$"

svTrkSumPtVsTrkSumPtProf :: YodaObj
svTrkSumPtVsTrkSumPtProf = yodaProf 10 0 100 "/svtrksumptvstrksumptprof" "$p_{\\mathrm T} \\sum \\mathrm{trk}$" "$<p_{\\mathrm T} \\sum \\mathrm{SV trk}>$"

bFragVsTrkSumPtProf :: YodaObj
bFragVsTrkSumPtProf = yodaProf 10 0 100 "/bfragvstrksumptprof" "$p_{\\mathrm T} \\sum \\mathrm{trk}$" "$<z_{p_{\\mathrm T}}>$"


nPVTrksHist :: YodaObj
nPVTrksHist = yodaHist 20 0 20 "/npvtrks" "$n$ PV tracks" $ dsigdXpbY "n" "1"

nSVTrksHist :: YodaObj
nSVTrksHist = yodaHist 10 0 10 "/nsvtrks" "$n$ SV tracks" $ dsigdXpbY "n" "1"

nPVTrksVsJetPtProf :: YodaObj
nPVTrksVsJetPtProf = yodaProf 18 25 250 "/npvtrksvsjetpt" "$p_{\\mathrm T}$ [GeV]" "$<n$ PV tracks $>$"

nSVTrksVsJetPtProf :: YodaObj
nSVTrksVsJetPtProf = yodaProf 18 25 250 "/nsvtrksvsjetpt" "$p_{\\mathrm T}$ [GeV]" "$<n$ SV tracks $>$"

