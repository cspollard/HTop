{-# LANGUAGE OverloadedStrings, TupleSections, TypeOperators, TypeFamilies, RankNTypes #-}

module Data.Atlas.Histograms where

import Control.Arrow
import Control.Applicative (liftA2)
import Data.Semigroup

import Data.Text (Text)

import Data.SGList
import Data.Histogram

import Data.Atlas.Event
import Data.Atlas.Jet
import Data.HEP.LorentzVector
import Data.HEP.YodaHisto

import Conduit
import qualified Data.Conduit.List as CL

import qualified Data.Vector as V

import Data.NumInstances ()

-- TODO
-- all of these lists should probably turn into (:.) lists for type checking

dsigdXpbY :: Text -> Text -> Text
dsigdXpbY x y = "$\\frac{d\\sigma}{d" <> x <> "} \\frac{\\mathrm{pb}}{" <> y <> "}$"

gev, rad, pt :: Text
gev = "\\mathrm{GeV}"
rad = "\\mathrm{rad}"
pt = "p_{\\mathrm{T}}"


ptHisto :: YodaHisto1D
ptHisto = YodaHisto "/pt" "$p_{\\mathrm T}$ [GeV]" (dsigdXpbY pt gev) $ histogram (constBin1D 25 (0, 1000)) mempty

eHisto :: YodaHisto1D
eHisto = YodaHisto "/E" "$E$ [GeV]" (dsigdXpbY "E" gev) $ histogram (constBin1D 25 (0, 1000)) mempty

mHisto :: YodaHisto1D
mHisto = YodaHisto "/mass" "mass [GeV]" (dsigdXpbY "m" gev) $ histogram (constBin1D 30 (0, 300)) mempty

etaHisto :: YodaHisto1D
etaHisto = YodaHisto "/eta" "$\\eta$" (dsigdXpbY "\\eta" rad) $ histogram (constBin1D 30 (-3, 3)) mempty

phiHisto :: YodaHisto1D
phiHisto = YodaHisto "/phi" "$\\phi$" (dsigdXpbY "\\phi" rad) $ histogram (constBin1D 30 (-pi, pi)) mempty

sd12Histo :: YodaHisto1D
sd12Histo = YodaHisto "/sd12" "$\\sqrt{d_{12}}$ [GeV]" (dsigdXpbY "\\sqrt{d_{12}}" gev) $ histogram (constBin1D 30 (0, 300)) mempty

dRHisto :: YodaHisto1D
dRHisto = YodaHisto "/deltaR" "$\\Delta R$" (dsigdXpbY "\\Delta R" "rad") $ histogram (constBin1D 25 (0, 5)) mempty

nObjHisto :: YodaHisto1D
nObjHisto = YodaHisto "/n" "multiplicity" (dsigdXpbY "n" "\\mathrm{unit}") $ histogram (constBin1D 25 (0, 5)) mempty

premap :: Monad m => (i -> j) -> ConduitM j o m r -> ConduitM i o m r
premap f c = CL.map f =$= c


(<<-) :: Monad m => ConduitM j o m r -> (i -> j) -> ConduitM i o m r
(<<-) = flip premap



filling :: (Monad m, Distribution d) => d -> Consumer (W d, X d) m d
filling = CL.fold fill

folding :: (Monad m, Functor f, Foldable f) => Consumer (Double, a) m r -> Consumer (Double, f a) m r
folding c = CL.mapFoldable (\(w, v) -> fmap (w,) v) =$= c


-- common histograms for LorentzVectors
lvHistos :: (Monad m, HasLorentzVector a) => Consumer (Double, a) m [YodaHisto1D]
lvHistos = sequenceConduits [ filling ptHisto <<- second ((Z :.) . (/ 1e3) . lvPt)
                            , filling etaHisto <<- second ((Z :.) . lvEta)
                            ] <<- second toPtEtaPhiE


jetHistos :: Monad m => Consumer (Double, Event) m [YodaHisto1D]
jetHistos = fmap (pathPrefix "/jets" . xlPrefix "small-$R$ jet ")
                <$> folding lvHistos <<- second eJets

-- TODO
-- not sure about this fixity.
infixr 3 =:=
(=:=) :: Monad m => ConduitM i o m r -> ConduitM i o m [r] -> ConduitM i o m [r]
c =:= cs = getZipConduit $ liftA2 (:) (ZipConduit c) (ZipConduit cs)

-- TODO
-- not sure about this fixity.
infixr 3 =++=
(=++=) :: Monad m => ConduitM i o m [r] -> ConduitM i o m [r] -> ConduitM i o m [r]
cs =++= cs' = getZipConduit $ liftA2 (++) (ZipConduit cs) (ZipConduit cs')


ljetSelection :: Event -> LargeJet -> Bool
ljetSelection evt lj = let es = eElectrons evt in
                            V.null es || minimum (sequenceA (fmap lvDR es) lj) > 1.0


ljetHistos :: Monad m => Consumer (Double, Event) m [YodaHisto1D]
ljetHistos = fmap (pathPrefix "/ljet0" . xlPrefix "leading large-$R$ jet ")
                <$> folding ljetHs <<- second (\evt -> leading . V.filter (ljetSelection evt) $ eLargeJets evt)

    where ljetHs = (filling mHisto <<- second ((Z :.) . (ljM / 1e3)))
                    =:= (filling sd12Histo <<- second ((Z :.) . (ljSD12 / 1e3)))
                    =:= lvHistos

tjetHistos :: Monad m => Consumer (Double, Event) m [YodaHisto1D]
tjetHistos = fmap (pathPrefix "/tjets" . xlPrefix "track jet ")
                <$> folding lvHistos <<- second eTrackJets

electronHistos :: Monad m => Consumer (Double, Event) m [YodaHisto1D]
electronHistos = fmap (pathPrefix "/electrons" . xlPrefix "electron ")
                <$> folding lvHistos <<- second eElectrons

muonHistos :: Monad m => Consumer (Double, Event) m [YodaHisto1D]
muonHistos = fmap (pathPrefix "/muons" . xlPrefix "muon ")
                <$> folding lvHistos <<- second eMuons

metHistos :: Monad m => Consumer (Double, Event) m [YodaHisto1D]
metHistos = fmap (pathPrefix "/met" . xlPrefix "$E_{\\mathrm{T}}^{\\mathrm{miss}}$ ")
                <$> lvHistos <<- second eMET

eventHistos :: Monad m => Consumer (Double, Event) m [YodaHisto1D]
eventHistos = fmap concat $ sequenceConduits [ jetHistos
                                             , ljetHistos
                                             , tjetHistos
                                             , electronHistos
                                             , muonHistos
                                             , metHistos
                                             ]

nominalHistos :: Monad m => Consumer Event m (SGList YodaHisto1D)
nominalHistos = SGList <$> eventHistos <<- (1.0,)


{-


eventSystHistos :: [Text] -> Builder Event [YodaHisto1D]
eventSystHistos = fmap concat . traverse eventHistos


channel :: Text -> (Event -> Bool) -> [Text] -> Builder Event (Text, [YodaHisto1D])
channel n f systs = fmap (n,) $ foldBuilder (eventSystHistos systs) <<- \e -> if f e then Just e else Nothing


-- TODO
-- event categorization could be cleaner.
channelSystHistos :: [Text] -> Builder Event [(Text, [YodaHisto1D])]
channelSystHistos systs = sequenceConduits [ channel "elelJ/" (\e -> V.length (eElectrons e) == 2 && V.length (eMuons e) == 0) systs
                                   , channel "elmuJ/" (\e -> V.length (eElectrons e) == 1 && V.length (eMuons e) == 1) systs
                                   , channel "elnuJ/" (\e -> V.length (eElectrons e) == 1 && V.length (eMuons e) == 0) systs
                                   , channel "mumuJ/" (\e -> V.length (eElectrons e) == 0 && V.length (eMuons e) == 2) systs
                                   , channel "munuJ/" (\e -> V.length (eElectrons e) == 0 && V.length (eMuons e) == 1) systs
                                   , channel "nunuJ/" (\e -> V.length (eElectrons e) == 0 && V.length (eMuons e) == 0) systs
                                   ]

-}
