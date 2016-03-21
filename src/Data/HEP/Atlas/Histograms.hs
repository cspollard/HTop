{-# LANGUAGE OverloadedStrings, FlexibleContexts, DeriveGeneric, TupleSections, TypeOperators, TypeFamilies, RankNTypes #-}

module Data.HEP.Atlas.Histograms where

import Control.Arrow
import Data.Monoid

import qualified Data.Vector as V

import Data.Text (Text)
import qualified Data.Text as T

import Data.Maybe (listToMaybe, fromJust)

import Data.Foldable (Foldable(..), toList)

import Data.Histogram

import Data.Serialize (Serialize(..))
import GHC.Generics (Generic)

import Data.HEP.Atlas.Event
import Data.HEP.LorentzVector

import qualified Data.Map as M

import Data.Conduit
import qualified Data.Conduit.List as CL
import Control.Monad.Catch (MonadThrow)


import Data.HEP.Atlas.Jet
import Data.HEP.Atlas.Electron
import Data.HEP.Atlas.Muon

ptHisto :: Histo1D
ptHisto = histogram (bin1D 50 (0, 500)) mempty

eHisto :: Histo1D
eHisto = histogram (bin1D 50 (0, 500)) mempty

mHisto :: Histo1D
mHisto = histogram (bin1D 50 (0, 200)) mempty

etaHisto :: Histo1D
etaHisto = histogram (bin1D 50 (-3, 3)) mempty

phiHisto :: Histo1D
phiHisto = histogram (bin1D 50 (-pi, pi)) mempty


-- a YodaHisto is just a histogram with some annotations.
data YodaHisto b val = YodaHisto { path :: Text
                                 , xLabel :: Text
                                 , yLabel :: Text
                                 , yhHisto :: !(Histogram b val)
                                 } deriving (Generic, Show)

type YodaHisto1D = YodaHisto (Bin1D Double) (Dist1D Double)

instance (Serialize val, Serialize b) => Serialize (YodaHisto b val) where

alterHisto :: (Histogram b val -> Histogram b val') -> YodaHisto b val -> YodaHisto b val'
alterHisto f (YodaHisto p xl yl h) = YodaHisto p xl yl $ f h

instance Functor (YodaHisto b) where
    -- fmap f (YodaHisto p xl yl h) = YodaHisto p xl yl $ fmap f h
    fmap f = alterHisto (fmap f)

instance ScaleW val => ScaleW (YodaHisto b val) where
    type W (YodaHisto b val) = W val
    yh `scaleW` w = (`scaleW` w) `alterHisto` yh

instance (Distribution val, Bin b, BinValue b ~ X val) => Distribution (YodaHisto b val) where
    type X (YodaHisto b val) = X val
    yh `fill` (w, x) = flip fill (w, x) `alterHisto` yh


xlPrefix :: Text -> YodaHisto b val -> YodaHisto b val
xlPrefix pre (YodaHisto p xl yl h) = YodaHisto p (pre <> xl) yl h

ylPrefix :: Text -> YodaHisto b val -> YodaHisto b val
ylPrefix pre (YodaHisto p xl yl h) = YodaHisto p xl (pre <> yl) h

pathPrefix :: Text -> YodaHisto b val -> YodaHisto b val
pathPrefix pre (YodaHisto p xl yl h) = YodaHisto (pre <> p) xl yl h

-- TODO
-- generalize
haddUnsafe :: (Eq b, Monoid val) => YodaHisto b val -> YodaHisto b val -> YodaHisto b val
haddUnsafe (YodaHisto _ _ _ h) (YodaHisto p xl yl h') = YodaHisto p xl yl (fromJust $ h `hadd` h')


premap :: MonadThrow m => (i -> j) -> ConduitM j o m r -> ConduitM i o m r
premap f c = CL.map f =$= c

(<<-) :: MonadThrow m => ConduitM j o m r -> (i -> j) -> ConduitM i o m r
(<<-) = flip premap


distSink :: (MonadThrow m, Distribution d) => d -> Consumer (W d, X d) m d
distSink = CL.fold fill

distSinkAll :: (MonadThrow m, Functor f, Foldable f)
            => Consumer (Double, a) m r -> Consumer (Double, f a) m r
distSinkAll c = CL.mapFoldable (\(w, v) -> fmap (w,) v) =$= c

-- TODO
-- need to figure out how to only fill first item.
{-
distSinkFirst :: (MonadThrow m, Foldable f)
            => Consumer (Double, a) m r -> Consumer (Double, f a) m r
distSinkFirst c = do
                    wv <- await
                    case wv of
                        Nothing -> yield Nothing >>= c
                        Just (w, v) -> case listToMaybe $ toList v of
                                            Nothing -> distSinkFirst c
                                            Just x -> distSinkFirst (yield (w, x) >>= c)
-}

-- TODO
-- still tons of boilerplate
-- TODO
-- instance Num b => Num (a -> b) where
ptHistoSink, eHistoSink, mHistoSink, etaHistoSink, phiHistoSink :: (LorentzVector l, MonadThrow m) => Consumer (Double, l) m YodaHisto1D
ptHistoSink = distSink (YodaHisto "pT" "$\\frac{d\\sigma}{d\\mathrm{GeV}}$" "$p_{\\mathrm T}$ [GeV]" ptHisto) <<- second ((Z :.) . (/ 1e3) . lvPt)
eHistoSink = distSink (YodaHisto "E" "$\\frac{d\\sigma}{d\\mathrm{GeV}}$" "$E$ [GeV]" eHisto) <<- second ((Z :.) . (/ 1e3) . lvE)
mHistoSink = distSink (YodaHisto "mass" "$\\frac{d\\sigma}{d\\mathrm{GeV}}$" "mass [GeV]" mHisto) <<- second ((Z :.) . (/ 1e3) . lvM)
etaHistoSink = distSink (YodaHisto "eta" "$\\frac{d\\sigma}{d\\eta}$" "$\\eta$" etaHisto) <<- second ((Z :.) . lvEta)
phiHistoSink = distSink (YodaHisto "phi" "$\\frac{d\\sigma}{d\\phi}$" "$\\phi$" phiHisto) <<- second ((Z :.) . lvPhi)


-- suite of histograms for LorentzVectors
lvHistos :: (HasLorentzVector a, MonadThrow m) => Consumer (Double, a) m [YodaHisto1D]
lvHistos = sequenceConduits [ptHistoSink, eHistoSink, mHistoSink, etaHistoSink, phiHistoSink]
                <<- second (lv :: HasLorentzVector a => a -> PtEtaPhiE)

jetHistos :: MonadThrow m => Consumer (Double, Event) m [YodaHisto1D]
jetHistos = (fmap (pathPrefix "/jets/" . xlPrefix "small-$R$ jet ") . concat)
                <$> sequenceConduits [distSinkAll lvHistos] <<- second eJets

ljetHistos :: MonadThrow m => Consumer (Double, Event) m [YodaHisto1D]
ljetHistos = (fmap (pathPrefix "/ljets/" . xlPrefix "large-$R$ jet ") . concat)
                <$> sequenceConduits [distSinkAll lvHistos] <<- second eLargeJets

tjetHistos :: MonadThrow m => Consumer (Double, Event) m [YodaHisto1D]
tjetHistos = (fmap (pathPrefix "/tjets/" . xlPrefix "track jet ") . concat)
                <$> sequenceConduits [distSinkAll lvHistos] <<- second eTrackJets

electronHistos :: MonadThrow m => Consumer (Double, Event) m [YodaHisto1D]
electronHistos = (fmap (pathPrefix "/electrons/" . xlPrefix "electron ") . concat)
                <$> sequenceConduits [distSinkAll lvHistos] <<- second eElectrons

muonHistos :: MonadThrow m => Consumer (Double, Event) m [YodaHisto1D]
muonHistos = (fmap (pathPrefix "/muons/" . xlPrefix "muon ") . concat)
                <$> sequenceConduits [distSinkAll lvHistos] <<- second eMuons

metHistos :: MonadThrow m => Consumer (Double, Event) m [YodaHisto1D]
metHistos = fmap (pathPrefix "/met/" . xlPrefix "$E_{\\mathrm{T}}^{\\mathrm{miss}}$ ")
                <$> lvHistos <<- second eMET

eventHistos :: MonadThrow m => Consumer (Double, Event) m [YodaHisto1D]
eventHistos = fmap concat $ sequenceConduits [ jetHistos
                                      , ljetHistos
                                      , tjetHistos
                                      , electronHistos
                                      , muonHistos
                                      , metHistos
                                      ]

nominalHistos :: MonadThrow m => Consumer Event m [YodaHisto1D]
nominalHistos = eventHistos <<- (weight "nominal" &&& id)

{-
                  fillAll (lvHistos (syst <> "/jets/") "small-$R$ jet ") <<- second eJets
                , fillAll (lvHistos (syst <> "/largejets/") "large-$R$ jet ") <<- second eLargeJets
                , fillAll (lvHistos (syst <> "/trackjets/") "track jet ") <<- second eTrackJets
                , fillAll (lvHistos (syst <> "/electrons/") "electron ") <<- second eElectrons
                , fillAll (lvHistos (syst <> "/muons/") "muon ") <<- second eMuons
                , fillFirst (lvHistos (syst <> "/jet0/") "leading small-$R$ jet ") <<- second eJets
                , fillFirst (lvHistos (syst <> "/largejet0/") "leading large-$R$ jet ") <<- second eLargeJets
                , fillAll (lvHistos (syst <> "/trackjet0/") "leading track jet ") <<- second eTrackJets
                , fillFirst (lvHistos (syst <> "/electron0/") "leading electron ") <<- second eElectrons
                , fillFirst (lvHistos (syst <> "/muon0/") "leading muon ") <<- second eMuons
                , lvHistos (syst <> "/met/") "$E_{\\mathrm T}^{\\mathrm miss}$ " <<- second eMET
                ] <<- (weight syst &&& id)


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

showHisto :: YodaHisto1D -> Text
showHisto (YodaHisto p xl yl h) = T.unlines $
                            [ "# BEGIN YODA_HISTO1D " <> p, "Path=" <> p, "Type=Histo1D"
                            , "XLabel=" <> xl, "YLabel=" <> yl
                            , "Total\tTotal\t" <> distToText (integral h)
                            , "Underflow\tUnderflow\t" <> distToText (underflow h)
                            , "Overflow\tOverflow\t" <> distToText (overflow h)
                            ] ++
                            map (\((Z :. xmin, Z :. xmax), b) -> T.pack (show xmin ++ "\t" ++ show xmax ++ "\t") <> distToText b) (toTuples h) ++
                            [ "# END YODA_HISTO1D", "" ]

                            where
                                distToText (Dist0 sw sw2 ne :. DistWX swx swx2) = T.pack $
                                                show sw ++ "\t" ++
                                                show sw2 ++ "\t" ++
                                                show swx ++ "\t" ++
                                                show swx2 ++ "\t" ++
                                                show ne
