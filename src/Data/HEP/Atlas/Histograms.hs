{-# LANGUAGE OverloadedStrings, DeriveGeneric, TupleSections, TypeOperators, TypeFamilies, RankNTypes #-}

module Data.HEP.Atlas.Histograms where

import Control.Arrow
import Data.Semigroup

import Data.Text (Text)


import Data.Histogram

import Data.Serialize (Serialize(..))
import GHC.Generics (Generic)

import Data.HEP.Atlas.Event
import Data.HEP.LorentzVector
import Data.HEP.YodaHisto

import Data.Conduit
import qualified Data.Conduit.List as CL
import Control.Monad.Catch (MonadThrow)


-- TODO
-- all of these lists should probably turn into (:.) lists for type checking

dsigdXpbY :: Text -> Text -> Text
dsigdXpbY x y = "$\\frac{d\\sigma}{d" <> x <> "} \\frac{\\mathrm{pb}}{" <> y <> "}$"

gev, rad, pt :: Text
gev = "\\mathrm{GeV}"
rad = "\\mathrm{rad}"
pt = "p_{\\mathrm{T}}"


ptHisto :: Histo1D
ptHisto = YodaHisto "/pt" "$p_{\\mathrm T}$ [GeV]" (dsigdXpbY pt gev) $ histogram (constBin1D 25 (0, 1000)) mempty

eHisto :: Histo1D
eHisto = YodaHisto "/E" "$E$ [GeV]" (dsigdXpbY "E" gev) $ histogram (constBin1D 25 (0, 1000)) mempty

mHisto :: Histo1D
mHisto = YodaHisto "/mass" "mass [GeV]" (dsigdXpbY "m" gev) $ histogram (constBin1D 30 (0, 300)) mempty

etaHisto :: Histo1D
etaHisto = YodaHisto "/eta" "$\\eta$" (dsigdXpbY "\\eta" rad) $ histogram (constBin1D 25 (-3, 3)) mempty

phiHisto :: Histo1D
phiHisto = YodaHisto "/phi" "$\\phi$" (dsigdXpbY "\\phi" rad) $ histogram (constBin1D 25 (-pi, pi)) mempty




premap :: MonadThrow m => (i -> j) -> ConduitM j o m r -> ConduitM i o m r
premap f c = CL.map f =$= c

(<<-) :: MonadThrow m => ConduitM j o m r -> (i -> j) -> ConduitM i o m r
(<<-) = flip premap


distSink :: (MonadThrow m, Distribution d) => d -> Consumer (W d, X d) m d
distSink = CL.fold fill

distSinkAll :: (MonadThrow m, Functor f, Foldable f)
            => Consumer (Double, a) m r -> Consumer (Double, f a) m r
distSinkAll c = CL.mapFoldable (\(w, v) -> fmap (w,) v) =$= c

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
-- still tons of boilerplate
-- TODO
-- instance Num b => Num (a -> b) where
ptHistoSink, eHistoSink, mHistoSink, etaHistoSink, phiHistoSink :: (LorentzVector l, MonadThrow m) => Consumer (Double, l) m YodaHisto1D

ptHistoSink = distSink ptHisto <<- second ((Z :.) . (/ 1e3) . lvPt)
eHistoSink = distSink eHisto <<- second ((Z :.) . (/ 1e3) . lvE)
mHistoSink = distSink mHisto <<- second ((Z :.) . (/ 1e3) . lvM)
etaHistoSink = distSink etaHisto <<- second ((Z :.) . lvEta)
phiHistoSink = distSink phiHisto <<- second ((Z :.) . lvPhi)


-- TODO
-- move to its own file

-- TODO
-- this should wrap all foldables if possible.

-- a list wrapper that is a semigroup *based on its inner type*, not
-- (++).
-- some things don't work that well, but it's "easy" for now.
newtype SGF h = SGList { fromSGList :: [h] }
                    deriving (Show, Generic)

instance Serialize h => Serialize (SHList h) where

liftSG :: ([a] -> [b]) -> SGList a -> SGList b
liftSG f (SGList xs) = SGList (f xs)

instance Functor SGList where
    fmap f = liftSG (fmap f)

instance Semigroup h => Semigroup (SGList h) where
    SGList xs <> SGList xs' = SGList $ zipWith (<>) xs xs'

instance Foldable SGList where
    foldr f x0 (SGList xs) = foldr f x0 xs

instance Traversable SGList where
    traverse f (SGList xs) = SGList <$> (traverse f xs)

instance ScaleW h => ScaleW (SGList h) where
    type W (SGList h) = W h
    scaleW hs w = fmap (flip scaleW w) hs

-- suite of histograms for LorentzVectors
lvHistos :: (HasLorentzVector a, MonadThrow m) => Consumer (Double, a) m [YodaHisto1D]
lvHistos = sequenceConduits [ptHistoSink, eHistoSink, mHistoSink, etaHistoSink, phiHistoSink]
                <<- second (lv :: HasLorentzVector a => a -> PtEtaPhiE)

jetHistos :: MonadThrow m => Consumer (Double, Event) m [YodaHisto1D]
jetHistos = (fmap (pathPrefix "/jets" . xlPrefix "small-$R$ jet ") . concat)
                <$> sequenceConduits [distSinkAll lvHistos] <<- second eJets

ljetHistos :: MonadThrow m => Consumer (Double, Event) m [YodaHisto1D]
ljetHistos = (fmap (pathPrefix "/ljets" . xlPrefix "large-$R$ jet ") . concat)
                <$> sequenceConduits [distSinkAll lvHistos] <<- second eLargeJets

tjetHistos :: MonadThrow m => Consumer (Double, Event) m [YodaHisto1D]
tjetHistos = (fmap (pathPrefix "/tjets" . xlPrefix "track jet ") . concat)
                <$> sequenceConduits [distSinkAll lvHistos] <<- second eTrackJets

electronHistos :: MonadThrow m => Consumer (Double, Event) m [YodaHisto1D]
electronHistos = (fmap (pathPrefix "/electrons" . xlPrefix "electron ") . concat)
                <$> sequenceConduits [distSinkAll lvHistos] <<- second eElectrons

muonHistos :: MonadThrow m => Consumer (Double, Event) m [YodaHisto1D]
muonHistos = (fmap (pathPrefix "/muons" . xlPrefix "muon ") . concat)
                <$> sequenceConduits [distSinkAll lvHistos] <<- second eMuons

metHistos :: MonadThrow m => Consumer (Double, Event) m [YodaHisto1D]
metHistos = fmap (pathPrefix "/met" . xlPrefix "$E_{\\mathrm{T}}^{\\mathrm{miss}}$ ")
                <$> lvHistos <<- second eMET

eventHistos :: MonadThrow m => Consumer (Double, Event) m [YodaHisto1D]
eventHistos = fmap concat $ sequenceConduits [ jetHistos
                                             , ljetHistos
                                             , tjetHistos
                                             , electronHistos
                                             , muonHistos
                                             , metHistos
                                             ]

nominalHistos :: MonadThrow m => Consumer Event m (SGList YodaHisto1D)
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
