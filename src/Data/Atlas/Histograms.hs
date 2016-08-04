{-# LANGUAGE OverloadedStrings, TupleSections, TypeOperators, TypeFamilies, RankNTypes #-}

module Data.Atlas.Histograms where

import Control.Arrow
import Control.Applicative (liftA2)
import Data.Semigroup

import Data.Text (Text)

import Data.SGList
import Data.Histogram
import Data.Histogram.Fill
import Data.Histogram.Funcs

import Data.Atlas.Event
import Data.Atlas.Helpers

import Conduit
import qualified Data.Conduit.List as CL

import qualified Data.Vector.Unboxed as V


-- TODO
-- all of these lists should probably turn into (:.) lists for type checking

type Weighted a = (Double, a)

dsigdXpbY :: Text -> Text -> Text
dsigdXpbY x y = "$\\frac{d\\sigma}{d" <> x <> "} \\frac{\\mathrm{pb}}{" <> y <> "}$"

gev, rad, pt :: Text
gev = "\\mathrm{GeV}"
rad = "\\mathrm{rad}"
pt = "p_{\\mathrm{T}}"


ptHisto :: Monad m => Consumer (Double, Double) m YodaHisto1D
ptHisto = YodaHisto "/pt" "$p_{\\mathrm T}$ [GeV]" (dsigdXpbY pt gev) <$> histCW (binD 0 25 1000)

eHisto :: Monad m => Consumer (Double, Double) m YodaHisto1D
eHisto = YodaHisto "/E" "<$>E<$> [GeV]" (dsigdXpbY "E" gev) <$> histCW (binD 0 25 1000)

mHisto :: Monad m => Consumer (Double, Double) m YodaHisto1D
mHisto = YodaHisto "/mass" "mass [GeV]" (dsigdXpbY "m" gev) <$> histCW (binD 0 30 300)

etaHisto :: Monad m => Consumer (Double, Double) m YodaHisto1D
etaHisto = YodaHisto "/eta" "<$>\\eta<$>" (dsigdXpbY "\\eta" rad) <$> histCW (binD (-3) 30 3)

phiHisto :: Monad m => Consumer (Double, Double) m YodaHisto1D
phiHisto = YodaHisto "/phi" "<$>\\phi<$>" (dsigdXpbY "\\phi" rad) <$> histCW (binD (-pi) 30 pi)

sd12Histo :: Monad m => Consumer (Double, Double) m YodaHisto1D
sd12Histo = YodaHisto "/sd12" "<$>\\sqrt{d_{12}}<$> [GeV]" (dsigdXpbY "\\sqrt{d_{12}}" gev) <$> histCW (binD 0 30 300)

dRHisto :: Monad m => Consumer (Double, Double) m YodaHisto1D
dRHisto = YodaHisto "/deltaR" "<$>\\Delta R<$>" (dsigdXpbY "\\Delta R" "rad") <$> histCW (binD 0 25 5)

nObjHisto :: Monad m => Consumer (Int, Double) m (YodaHisto BinI Double)
nObjHisto = YodaHisto "/n" "multiplicity" (dsigdXpbY "n" "\\mathrm{unit}") <$> histCW (binI 0 5)


histCW :: (Monad m, Num v, V.Unbox v, Bin b) => b -> Consumer (BinValue b, v) m (Histogram b v)
histCW bin = let h = histogram bin (V.replicate (nBins bin) 0)
             in  go h
    where go h' = do x <- await
                     case x of
                        Just y -> go (fill (+) y h')
                        Nothing -> return h'

-- common histograms for LorentzVectors
lvHistos :: (Monad m, HasLorentzVector a) => Consumer (Weighted a) m [YodaHisto1D]
lvHistos = sequenceConduits [ filling ptHisto <<- second ((Z :.) . (/ 1e3) . lvPt)
                            , filling etaHisto <<- second ((Z :.) . lvEta)
                            ] <<- second toPtEtaPhiE


jetHistos :: Monad m => Consumer (Weighted Event) m [YodaHisto1D]
jetHistos = fmap (pathPrefix "/jets" . xlPrefix "small-$R$ jet ")
                <$> folding lvHistos <<- second eJets


ljetHistos :: Monad m => Consumer (Weighted Event) m [YodaHisto1D]
ljetHistos = fmap (pathPrefix "/ljet0" . xlPrefix "leading large-$R$ jet ")
                <$> folding ljetHs <<- second eLargeJets

    where ljetHs = (filling mHisto <<- second ((Z :.) . (ljM / 1e3)))
                    =:= (filling sd12Histo <<- second ((Z :.) . (ljSD12 / 1e3)))
                    =:= lvHistos

eljetHistos :: Monad m => Consumer (Weighted Event) m YodaHisto1D
eljetHistos = (pathPrefix "/elljet0" . xlPrefix "electron-leading large-$R$ jet ")
                <$> folding (folding (filling dRHisto <<- second (Z :.)))
                <<- second (\evt -> fmap (flip minDR (eElectrons evt)) . leading $ eLargeJets evt)

tjetHistos :: Monad m => Consumer (Weighted Event) m [YodaHisto1D]
tjetHistos = fmap (pathPrefix "/tjets" . xlPrefix "track jet ")
                <$> folding lvHistos <<- second eTrackJets

electronHistos :: Monad m => Consumer (Weighted Event) m [YodaHisto1D]
electronHistos = fmap (pathPrefix "/electrons" . xlPrefix "electron ")
                <$> folding lvHistos <<- second eElectrons

muonHistos :: Monad m => Consumer (Weighted Event) m [YodaHisto1D]
muonHistos = fmap (pathPrefix "/muons" . xlPrefix "muon ")
                <$> folding lvHistos <<- second eMuons

metHistos :: Monad m => Consumer (Weighted Event) m [YodaHisto1D]
metHistos = fmap (pathPrefix "/met" . xlPrefix "$E_{\\mathrm{T}}^{\\mathrm{miss}}$ ")
                <$> lvHistos <<- second eMET

eventHistos :: Monad m => Consumer (Weighted Event) m [YodaHisto1D]
eventHistos = eljetHistos =:= jetHistos =++= ljetHistos
                          =++= tjetHistos =++= electronHistos
                          =++= muonHistos =++= metHistos


channel :: Monad m => Text -> (Event -> Bool) -> Consumer (Weighted Event) m [YodaHisto1D]
channel n f = fmap (fmap (pathPrefix n)) $ filterC (f . snd) =$= eventHistos


channelHistos :: Monad m => Consumer (Weighted Event) m (SGList YodaHisto1D)
channelHistos = SGList . concat <$> sequenceConduits [ channel "/elelJ/inclusive" elelJ
                                                     , channel "/elelJ/0tag0addtag" (and . sequenceA [elelJ, (== 0) . nTags])
                                                     , channel "/elmuJ" elmuJ
                                                     , channel "/elnuJ" elmuJ
                                                     , channel "/mumuJ" mumuJ
                                                     , channel "/munuJ" munuJ
                                                     , channel "/nunuJ" nunuJ
                                                     ]
