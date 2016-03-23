{-# LANGUAGE OverloadedStrings, TupleSections, TypeOperators, TypeFamilies, RankNTypes #-}

module Data.HEP.Atlas.Histograms where

import Control.Arrow
import Data.Semigroup

import Data.Text (Text)

import Data.SGList
import Data.Histogram

import Data.HEP.Atlas.Event
import Data.HEP.LorentzVector
import Data.HEP.YodaHisto

import Conduit
import qualified Data.Conduit.List as CL


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
etaHisto = YodaHisto "/eta" "$\\eta$" (dsigdXpbY "\\eta" rad) $ histogram (constBin1D 25 (-3, 3)) mempty

phiHisto :: YodaHisto1D
phiHisto = YodaHisto "/phi" "$\\phi$" (dsigdXpbY "\\phi" rad) $ histogram (constBin1D 25 (-pi, pi)) mempty




premap :: Monad m => (i -> j) -> ConduitM j o m r -> ConduitM i o m r
premap f c = CL.map f =$= c

(<<-) :: Monad m => ConduitM j o m r -> (i -> j) -> ConduitM i o m r
(<<-) = flip premap


distSink :: (Monad m, Distribution d) => d -> Consumer (W d, X d) m d
distSink = CL.fold fill

distSinkAll :: (Monad m, Functor f, Foldable f)
            => Consumer (Double, a) m r -> Consumer (Double, f a) m r
distSinkAll c = CL.mapFoldable (\(w, v) -> fmap (w,) v) =$= c


-- TODO
-- still need to do leading...
{-
distSinkFirst :: (MonadThrow m, Foldable f)
            => Consumer (Double, a) m r -> Consumer (Double, f a) m r
distSinkFirst c = do wv <- await
                     case wv of
                        Nothing -> c
                        Just (w, v) -> case toList v of
                            []    -> distSinkFirst c
                            x : _ -> distSinkFirst (yield (w, x) =$= c)
-}


-- TODO
-- instance Num b => Num (a -> b) where
-- suite of histograms for LorentzVectors
lvHistos :: (HasLorentzVector a, Monad m) => Consumer (Double, a) m [YodaHisto1D]
lvHistos = sequenceConduits [ distSink ptHisto <<- second ((Z :.) . (/ 1e3) . lvPt)
                            , distSink eHisto <<- second ((Z :.) . (/ 1e3) . lvE)
                            , distSink mHisto <<- second ((Z :.) . (/ 1e3) . lvM)
                            , distSink etaHisto <<- second ((Z :.) . lvEta)
                            , distSink phiHisto <<- second ((Z :.) . lvPhi)
                            ] <<- second (lv :: HasLorentzVector a => a -> PtEtaPhiE)

jetHistos :: Monad m => Consumer (Double, Event) m [YodaHisto1D]
jetHistos = (fmap (pathPrefix "/jets" . xlPrefix "small-$R$ jet ") . concat)
                <$> sequenceConduits [distSinkAll lvHistos] <<- second eJets

ljetHistos :: Monad m => Consumer (Double, Event) m [YodaHisto1D]
ljetHistos = (fmap (pathPrefix "/ljets" . xlPrefix "large-$R$ jet ") . concat)
                <$> sequenceConduits [distSinkAll lvHistos] <<- second eLargeJets

tjetHistos :: Monad m => Consumer (Double, Event) m [YodaHisto1D]
tjetHistos = (fmap (pathPrefix "/tjets" . xlPrefix "track jet ") . concat)
                <$> sequenceConduits [distSinkAll lvHistos] <<- second eTrackJets

electronHistos :: Monad m => Consumer (Double, Event) m [YodaHisto1D]
electronHistos = (fmap (pathPrefix "/electrons" . xlPrefix "electron ") . concat)
                <$> sequenceConduits [distSinkAll lvHistos] <<- second eElectrons

muonHistos :: Monad m => Consumer (Double, Event) m [YodaHisto1D]
muonHistos = (fmap (pathPrefix "/muons" . xlPrefix "muon ") . concat)
                <$> sequenceConduits [distSinkAll lvHistos] <<- second eMuons

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
