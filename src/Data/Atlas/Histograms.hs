{-# LANGUAGE OverloadedStrings, TupleSections, TypeOperators, TypeFamilies, RankNTypes #-}

module Data.Atlas.Histograms where

import Control.Lens

import Conduit
import qualified Data.Conduit.List as CL

import Data.Maybe (listToMaybe)
import Data.Foldable (toList)

import Control.Applicative (liftA2, ZipList(..))

import Data.Semigroup

import Data.Text (Text)

import Data.YODA.Histo
import Data.YODA.Profile

import Data.Atlas.Event
import Data.Atlas.Selection


-- TODO
-- all of these lists should probably turn into (:.) lists for type checking

type WithWeight a = (Double, a)

dsigdXpbY :: Text -> Text -> Text
dsigdXpbY x y = "$\\frac{d\\sigma}{d" <> x <> "} \\frac{\\mathrm{pb}}{" <> y <> "}$"

gev, rad, pt :: Text
gev = "\\mathrm{GeV}"
rad = "\\mathrm{rad}"
pt = "p_{\\mathrm{T}}"


ptHisto :: YodaHisto1D
ptHisto = yodaHisto1D 25 0 1000 & annots . at "Path" ?~ "/pt"
                                & annots . at "XLabel" ?~ "$p_{\\mathrm T}$ [GeV]"
                                & annots . at "YLabel" ?~ dsigdXpbY pt gev

sumTrkPtHisto :: YodaHisto1D
sumTrkPtHisto = yodaHisto1D 25 0 500 & annots . at "Path" ?~ "/sumtrkpt"
                                     & annots . at "XLabel" ?~ "$\\sum_{\\mathrm{trk}} p_{\\mathrm T}$ [GeV]"
                                     & annots . at "YLabel" ?~ dsigdXpbY pt gev

sumSVTrkPtHisto :: YodaHisto1D
sumSVTrkPtHisto = yodaHisto1D 25 0 500 & annots . at "Path" ?~ "/sumsvtrkpt"
                                       & annots . at "XLabel" ?~ "SV $\\sum_{\\mathrm{trk}} p_{\\mathrm T}$ [GeV]"
                                       & annots . at "YLabel" ?~ dsigdXpbY pt gev

bFragHisto :: YodaHisto1D
bFragHisto = yodaHisto1D 22 0 1.1 & annots . at "Path" ?~ "/bFrag"
                                  & annots . at "XLabel" ?~ "$z_{p_{\\mathrm T}}$"
                                  & annots . at "YLabel" ?~ dsigdXpbY "z_{p_{\\mathrm T}}" "1" 

eHisto :: YodaHisto1D
eHisto = yodaHisto1D 25 0 1000 & annots . at "Path" ?~ "/E"
                               & annots . at "XLabel" ?~ "$E$ [GeV]"
                               & annots . at "YLabel" ?~ dsigdXpbY "E" gev

mHisto :: YodaHisto1D
mHisto = yodaHisto1D 30 0 300 & annots . at "Path" ?~ "/mass"
                              & annots . at "XLabel" ?~ "mass [GeV]"
                              & annots . at "YLabel" ?~ dsigdXpbY "m" gev

etaHisto :: YodaHisto1D
etaHisto = yodaHisto1D 30 (-3) 3 & annots . at "Path" ?~ "/eta"
                                 & annots . at "XLabel" ?~ "$\\eta$"
                                 & annots . at "YLabel" ?~ dsigdXpbY "\\eta" rad

phiHisto :: YodaHisto1D
phiHisto = yodaHisto1D 30 (-pi) pi & annots . at "Path" ?~ "/phi"
                                   & annots . at "XLabel" ?~ "$\\phi$"
                                   & annots . at "YLabel" ?~ dsigdXpbY "\\phi" rad

{-
sd12Histo :: YodaHisto1D
sd12Histo = yodaHisto1D "/sd12" "$\\sqrt{d_{12}}$ [GeV]" (dsigdXpbY "\\sqrt{d_{12}}" gev) $ histo1D (binD 0 30 300)

tau21Histo :: YodaHisto1D
tau21Histo = yodaHisto1D "/tau21" "$\\tau_{21}$" (dsigdXpbY "\\tau_{21}" "1") $ histo1D (binD 0 30 30)

tau32Histo :: YodaHisto1D
tau32Histo = yodaHisto1D "/tau32" "$\\tau_{32}$" (dsigdXpbY "\\tau_{32}" "1") $ histo1D (binD 0 30 30)

dRHisto :: YodaHisto1D
dRHisto = yodaHisto1D "/deltaR" "$\\Delta R$" (dsigdXpbY "\\Delta R" "rad") $ histo1D (binD 0 25 5)

nObjHisto :: YodaHisto1D
nObjHisto = yodaHisto1D "/n" "multiplicity" (dsigdXpbY "n" "\\mathrm{unit}") $ histo1D (binD 0 5 5)
-}


filling :: (Monad m, Fillable a) => a -> Consumer (FillVec a) m a
filling = foldlC (flip fill)

fillingOver :: (Monad m, Fillable a) => Lens' b a -> b -> Consumer (FillVec a) m b
fillingOver l = foldlC (\h' x -> over l (fill x) h')

fillAll :: (Monad m, Applicative f, Foldable f)
         => Consumer (v, a) m b -> Consumer (v, f a) m b
fillAll c = CL.map sequenceA =$= CL.concat =$= c

fillFirst :: (Monad m, Applicative f, Foldable f)
          => Consumer (v, a) m b -> Consumer (v, f a) m b
fillFirst c = CL.map (traverse firstF) =$= CL.concat =$= c

firstF :: Foldable f => f a -> Maybe a
firstF = listToMaybe . toList


infixr 2 <=$=
(<=$=) :: Monad m => ConduitM b c m r -> Conduit a m b -> ConduitM a c m r
(<=$=) = flip (=$=)

-- not sure about this fixity.
infixr 3 =:=
(=:=) :: Monad m => ConduitM i o m r -> ConduitM i o m [r] -> ConduitM i o m [r]
c =:= cs = getZipConduit $ liftA2 (:) (ZipConduit c) (ZipConduit cs)

infixr 3 =++=
(=++=) :: Monad m => ConduitM i o m [r] -> ConduitM i o m [r] -> ConduitM i o m [r]
cs =++= cs' = getZipConduit $ liftA2 (++) (ZipConduit cs) (ZipConduit cs')


-- common histograms for LorentzVectors
lvHistos :: (Monad m, HasLorentzVector a) => Consumer (WithWeight a) m [YodaHisto1D]
lvHistos = sequenceConduits [ fillingOver thing ptHisto  <=$= CL.map (fmap lvPt)
                            , fillingOver thing etaHisto <=$= CL.map (fmap lvEta)
                            ] <=$= CL.map (fmap toPtEtaPhiE)

jetTrkHistos :: Monad m => Consumer (WithWeight Jet) m [YodaHisto1D]
jetTrkHistos = sequenceConduits [ fillingOver thing sumTrkPtHisto    <=$= CL.map (fmap sumTrkPt)
                                , fillingOver thing sumSVTrkPtHisto  <=$= CL.map (fmap sumSVTrkPt)
                                , fillingOver thing bFragHisto       <=$= CL.map (fmap bFrag)
                                ]

jetsHistos :: Monad m => Consumer (WithWeight [Jet]) m [YodaHisto1D]
jetsHistos = fmap ((path %~ ("/jets" <>)) . (xlabel %~ ("small-$R$ jet " <>)))
             <$> (fillAll jetTrkHistos =++= fillAll lvHistos)

jet0Histos :: Monad m => Consumer (WithWeight [Jet]) m [YodaHisto1D]
jet0Histos = fmap ((path %~ ("/jet0" <>)) . (xlabel %~ ("leading small-$R$ jet " <>)))
             <$> (fillFirst jetTrkHistos =++= fillFirst lvHistos)

probeJetHistos :: Monad m => Consumer (WithWeight [Jet]) m [YodaHisto1D]
probeJetHistos = fmap ((path %~ ("/probejet" <>)) . (xlabel %~ ("probe small-$R$ jet " <>)))
             <$> (fillAll jetTrkHistos =++= fillAll lvHistos) <=$= CL.map (fmap probeJets)

jetHistos :: Monad m => Consumer (WithWeight Event) m [YodaHisto1D]
jetHistos = (jetsHistos =++= jet0Histos =++= probeJetHistos) <=$= CL.map (fmap _jets)

{-
ljetHs :: Monad m => Consumer (WithWeight LargeJet) m [YodaHisto1D]
ljetHs = (fillingYH mHisto <=$= CL.map (fmap ljM))
         =:= (fillingYH sd12Histo <=$= CL.map (fmap ljSD12))
         =:= lvHistos


ljetsHistos :: Monad m => Consumer (WithWeight LargeJets) m [YodaHisto1D]
ljetsHistos = fmap ((path %~ ("/ljets" <>)) . (xlabel %~ ("large-$R$ jet " <>)))
              <$> fillAll ljetHs


ljet0Histos :: Monad m => Consumer (WithWeight LargeJets) m [YodaHisto1D]
ljet0Histos = fmap ((path %~ ("/ljet0" <>)) . (xlabel %~ ("leading large-$R$ jet " <>)))
              <$> fillFirst ljetHs


ljetHistos :: Monad m => Consumer (WithWeight Event) m [YodaHisto1D]
ljetHistos = (ljetsHistos =++= ljet0Histos) <=$= CL.map (fmap eLargeJets)


eljetHisto :: Monad m => Consumer (WithWeight Event) m YodaHisto1D
eljetHisto = ((path %~ ("/elljet0" <>)) . (xlabel %~ ("electron-large-$R$ jet " <>)))
             <$> fillFirst (fillingYH dRHisto) <=$= CL.map (fmap f)
    where f evt = flip minDR (eElectrons evt) =<< leading (eLargeJets evt)

tjetHistos :: Monad m => Consumer (WithWeight Event) m [YodaHisto1D]
tjetHistos = fmap ((path %~ ("/tjets" <>)) . (xlabel %~ ("track jet " <>)))
             <$> fillAll lvHistos <=$= CL.map (fmap eTrackJets)
-}

electronsHistos :: Monad m => Consumer (WithWeight Event) m [YodaHisto1D]
electronsHistos = fmap ((path %~ ("/electrons" <>)) . (xlabel %~ ("electron " <>)))
                  <$> fillAll lvHistos <=$= CL.map (fmap _electrons)


muonsHistos :: Monad m => Consumer (WithWeight Event) m [YodaHisto1D]
muonsHistos = fmap ((path %~ ("/muons" <>)) . (xlabel %~ ("muon " <>)))
              <$> fillAll lvHistos <=$= CL.map (fmap _muons)

metHisto :: Monad m => Consumer (WithWeight Event) m YodaHisto1D
metHisto = ((path %~ ("/met" <>)) . (xlabel %~ ("$E_{\\mathrm{T}}^{\\mathrm{miss}}$ " <>)))
             <$> fillingOver thing ptHisto <=$= CL.map (fmap (lvPt . _met))

eventHistos :: Monad m => Consumer (WithWeight Event) m [YodaHisto1D]
eventHistos = {- eljetHisto =:= -} metHisto =:= jetHistos
                         =++= electronsHistos =++= muonsHistos
                         -- =++= ljetHistos =++= tjetHistos


channel :: Monad m => Text -> (Event -> Bool) -> Consumer (WithWeight Event) m [YodaHisto1D]
channel n f = (fmap.fmap) (path %~ (n <>)) $ filterC (f . snd) =$= eventHistos


channelHistos :: Monad m => Consumer (WithWeight Event) m (Int, ZipList YodaHisto1D)
channelHistos = getZipConduit $ (,) <$> ZipConduit lengthC <*> ZipConduit (ZipList . concat <$> sequenceConduits [ CL.map (fmap pruneJets) =$= channel "/elmujj/inclusive" elmujj ])
