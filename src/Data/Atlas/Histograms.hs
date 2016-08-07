{-# LANGUAGE OverloadedStrings, TupleSections, TypeOperators, TypeFamilies, RankNTypes #-}

module Data.Atlas.Histograms where

import Control.Arrow
import Control.Applicative (liftA2)
import Data.Semigroup

import Data.Text (Text)

import Data.SGList
import Data.Histogram
import Data.HEP.YodaHisto

import Data.Atlas.Event
import Data.Atlas.Helpers

import Conduit
import qualified Data.Conduit.List as CL

import qualified Data.Vector as V

import Data.NumInstances ()


-- TODO
-- all of these lists should probably turn into (:.) lists for type checking

type Weighted a = (Double, a)

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

tau21Histo :: YodaHisto1D
tau21Histo = YodaHisto "/tau21" "$\\tau_{21}$" (dsigdXpbY "\\tau_{21}" "1") $ histogram (constBin1D 30 (0, 30)) mempty

tau32Histo :: YodaHisto1D
tau32Histo = YodaHisto "/tau32" "$\\tau_{32}$" (dsigdXpbY "\\tau_{32}" "1") $ histogram (constBin1D 30 (0, 30)) mempty

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

folding :: (Monad m, Functor f, Foldable f) => Consumer (Weighted a) m r -> Consumer (Weighted (f a)) m r
folding c = CL.mapFoldable (\(w, v) -> fmap (w,) v) =$= c


-- common histograms for LorentzVectors
lvHistos :: (Monad m, HasLorentzVector a) => Consumer (Weighted a) m [YodaHisto1D]
lvHistos = sequenceConduits [ filling ptHisto <<- second ((Z :.) . (/ 1e3) . lvPt)
                            , filling etaHisto <<- second ((Z :.) . lvEta)
                            ] <<- second toPtEtaPhiE


jetHistos :: Monad m => Consumer (Weighted Event) m [YodaHisto1D]
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


ljetHistos :: Monad m => Consumer (Weighted Event) m [YodaHisto1D]
ljetHistos = fmap (pathPrefix "/ljet0" . xlPrefix "leading large-$R$ jet ")
                <$> folding ljetHs <<- second eLargeJets

    where ljetHs = (filling mHisto <<- second ((Z :.) . (ljM / 1e3)))
                    =:= (filling sd12Histo <<- second ((Z :.) . (ljSD12 / 1e3)))
                    -- TODO
                    -- tau21 and tau32
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


both :: (Bool, Bool) -> Bool
both = uncurry (&&)


channelHistos :: Monad m => Consumer (Weighted Event) m (SGList YodaHisto1D)
channelHistos = SGList . concat <$> sequenceConduits [ channel "/elelJ/inclusive" elelJ
                                                     , channel "/elelJ/0tag0addtag" (and . sequenceA [elelJ, (== (0, 0)) . nTags])
                                                     , channel "/elelJ/1tag0addtag" (and . sequenceA [elelJ, (== (1, 0)) . nTags])
                                                     , channel "/elelJ/2tag0addtag" (and . sequenceA [elelJ, (== (2, 0)) . nTags])
                                                     , channel "/elelJ/0tag1addtag" (and . sequenceA [elelJ, (== (0, 1)) . nTags])
                                                     , channel "/elelJ/1tag1addtag" (and . sequenceA [elelJ, (== (1, 1)) . nTags])
                                                     , channel "/elelJ/2tag1addtag" (and . sequenceA [elelJ, (== (2, 1)) . nTags])
                                                     , channel "/elmuJ/inclusive" elmuJ
                                                     , channel "/elmuJ/0tag0addtag" (and . sequenceA [elmuJ, (== (0, 0)) . nTags])
                                                     , channel "/elmuJ/1tag0addtag" (and . sequenceA [elmuJ, (== (1, 0)) . nTags])
                                                     , channel "/elmuJ/2tag0addtag" (and . sequenceA [elmuJ, (== (2, 0)) . nTags])
                                                     , channel "/elmuJ/0tag1addtag" (and . sequenceA [elmuJ, (== (0, 1)) . nTags])
                                                     , channel "/elmuJ/1tag1addtag" (and . sequenceA [elmuJ, (== (1, 1)) . nTags])
                                                     , channel "/elmuJ/2tag1addtag" (and . sequenceA [elmuJ, (== (2, 1)) . nTags])
                                                     , channel "/mumuJ/inclusive" mumuJ
                                                     , channel "/mumuJ/0tag0addtag" (and . sequenceA [mumuJ, (== (0, 0)) . nTags])
                                                     , channel "/mumuJ/1tag0addtag" (and . sequenceA [mumuJ, (== (1, 0)) . nTags])
                                                     , channel "/mumuJ/2tag0addtag" (and . sequenceA [mumuJ, (== (2, 0)) . nTags])
                                                     , channel "/mumuJ/0tag1addtag" (and . sequenceA [mumuJ, (== (0, 1)) . nTags])
                                                     , channel "/mumuJ/1tag1addtag" (and . sequenceA [mumuJ, (== (1, 1)) . nTags])
                                                     , channel "/mumuJ/2tag1addtag" (and . sequenceA [mumuJ, (== (2, 1)) . nTags])
                                                     , channel "/elnuJ/inclusive" elnuJ
                                                     , channel "/elnuJ/0tag0addtag" (and . sequenceA [elnuJ, (== (0, 0)) . nTags])
                                                     , channel "/elnuJ/1tag0addtag" (and . sequenceA [elnuJ, (== (1, 0)) . nTags])
                                                     , channel "/elnuJ/2tag0addtag" (and . sequenceA [elnuJ, (== (2, 0)) . nTags])
                                                     , channel "/elnuJ/0tag1addtag" (and . sequenceA [elnuJ, (== (0, 1)) . nTags])
                                                     , channel "/elnuJ/1tag1addtag" (and . sequenceA [elnuJ, (== (1, 1)) . nTags])
                                                     , channel "/elnuJ/2tag1addtag" (and . sequenceA [elnuJ, (== (2, 1)) . nTags])
                                                     , channel "/munuJ/inclusive" munuJ
                                                     , channel "/munuJ/0tag0addtag" (and . sequenceA [munuJ, (== (0, 0)) . nTags])
                                                     , channel "/munuJ/1tag0addtag" (and . sequenceA [munuJ, (== (1, 0)) . nTags])
                                                     , channel "/munuJ/2tag0addtag" (and . sequenceA [munuJ, (== (2, 0)) . nTags])
                                                     , channel "/munuJ/0tag1addtag" (and . sequenceA [munuJ, (== (0, 1)) . nTags])
                                                     , channel "/munuJ/1tag1addtag" (and . sequenceA [munuJ, (== (1, 1)) . nTags])
                                                     , channel "/munuJ/2tag1addtag" (and . sequenceA [munuJ, (== (2, 1)) . nTags])
                                                     , channel "/nunuJ/inclusive" nunuJ
                                                     , channel "/nunuJ/0tag0addtag" (and . sequenceA [nunuJ, (== (0, 0)) . nTags])
                                                     , channel "/nunuJ/1tag0addtag" (and . sequenceA [nunuJ, (== (1, 0)) . nTags])
                                                     , channel "/nunuJ/2tag0addtag" (and . sequenceA [nunuJ, (== (2, 0)) . nTags])
                                                     , channel "/nunuJ/0tag1addtag" (and . sequenceA [nunuJ, (== (0, 1)) . nTags])
                                                     , channel "/nunuJ/1tag1addtag" (and . sequenceA [nunuJ, (== (1, 1)) . nTags])
                                                     , channel "/nunuJ/2tag1addtag" (and . sequenceA [nunuJ, (== (2, 1)) . nTags])
                                                     ]
