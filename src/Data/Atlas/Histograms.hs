{-# LANGUAGE OverloadedStrings, TupleSections, TypeOperators, TypeFamilies, RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Data.Atlas.Histograms where

import Control.Lens

import Conduit
import qualified Data.Conduit.List as CL

import Data.Maybe (listToMaybe)
import Data.Foldable (toList)

import Control.Applicative (liftA2, ZipList(..))

import Data.Semigroup

import Data.Text (Text)

import Data.YODA.Obj

import Data.Atlas.Event
import Data.Atlas.Selection


-- TODO
-- all of these lists should probably turn into (:.) lists for type checking


filling :: (Monad m, Fillable a) => a -> Consumer (FillVec a) m a
filling = foldlC (flip fill)

fillingOver :: (Monad m, Fillable a) => Traversal' b a -> b -> Consumer (FillVec a) m b
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

dsigdXpbY :: Text -> Text -> Text
dsigdXpbY x y = "$\\frac{d\\sigma}{d" <> x <> "} \\frac{\\mathrm{pb}}{" <> y <> "}$"

gev, rad, pt :: Text
gev = "\\mathrm{GeV}"
rad = "\\mathrm{rad}"
pt = "p_{\\mathrm{T}}"


-- common histograms for LorentzVectors

ptHist :: YodaObj
ptHist = yodaHist1D 25 0 500
    & annots . at "Path" ?~ "/pt"
    & annots . at "XLabel" ?~ "$p_{\\mathrm T}$ [GeV]"
    & annots . at "YLabel" ?~ dsigdXpbY pt gev

etaHist :: YodaObj
etaHist = yodaHist1D 30 (-3) 3
    & annots . at "Path" ?~ "/eta"
    & annots . at "XLabel" ?~ "$\\eta$"
    & annots . at "YLabel" ?~ dsigdXpbY "\\eta" rad

phiHist :: YodaObj
phiHist = yodaHist1D 30 (-pi) pi
    & annots . at "Path" ?~ "/phi"
    & annots . at "XLabel" ?~ "$\\phi$"
    & annots . at "YLabel" ?~ dsigdXpbY "\\phi" rad

eHist :: YodaObj
eHist = yodaHist1D 25 0 500
    & annots . at "Path" ?~ "/E"
    & annots . at "XLabel" ?~ "$E$ [GeV]"
    & annots . at "YLabel" ?~ dsigdXpbY "E" gev

mHist :: YodaObj
mHist = yodaHist1D 30 0 300
    & annots . at "Path" ?~ "/mass"
    & annots . at "XLabel" ?~ "mass [GeV]"
    & annots . at "YLabel" ?~ dsigdXpbY "m" gev

lvObjs :: (Monad m, HasLorentzVector a) => Consumer (WithWeight a) m [YodaObj]
lvObjs = sequenceConduits [ fillingOver (noted . _H1DD) ptHist  <=$= CL.map (fmap lvPt)
                          , fillingOver (noted . _H1DD) etaHist <=$= CL.map (fmap lvEta)
                          ] <=$= CL.map (fmap toPtEtaPhiE)



sumTrkPtHist :: YodaObj
sumTrkPtHist = yodaHist1D 25 0 500
    & annots . at "Path" ?~ "/sumtrkpt"
    & annots . at "XLabel" ?~ "$\\sum_{\\mathrm{trk}} p_{\\mathrm T}$ [GeV]"
    & annots . at "YLabel" ?~ dsigdXpbY pt gev

sumSVTrkPtHist :: YodaObj
sumSVTrkPtHist = yodaHist1D 25 0 500
    & annots . at "Path" ?~ "/sumsvtrkpt"
    & annots . at "XLabel" ?~ "SV $\\sum_{\\mathrm{trk}} p_{\\mathrm T}$ [GeV]"
    & annots . at "YLabel" ?~ dsigdXpbY pt gev

bFragHist :: YodaObj
bFragHist = yodaHist1D 22 0 1.1
    & annots . at "Path" ?~ "/bfrag"
    & annots . at "XLabel" ?~ "$z_{p_{\\mathrm T}}$"
    & annots . at "YLabel" ?~ dsigdXpbY "z_{p_{\\mathrm T}}" "1" 

sumTrkPtVsJetPtProf :: YodaObj
sumTrkPtVsJetPtProf = yodaProf1D 18 25 250
    & annots . at "Path" ?~ "/sumtrkptprof"
    & annots . at "XLabel" ?~ "$p_{\\mathrm T}$ [GeV]"
    & annots . at "YLabel" ?~ "$<\\sum_{\\mathrm{trk}} p_{\\mathrm T}>$"

sumSVTrkPtVsJetPtProf :: YodaObj
sumSVTrkPtVsJetPtProf = yodaProf1D 18 25 250
    & annots . at "Path" ?~ "/sumsvtrkptprof"
    & annots . at "XLabel" ?~ "$p_{\\mathrm T}$ [GeV]"
    & annots . at "YLabel" ?~ "$<\\mathrm{SV }\\sum_{\\mathrm{trk}} p_{\\mathrm T}>$"

bFragVsJetPtProf :: YodaObj
bFragVsJetPtProf = yodaProf1D 18 25 250
    & annots . at "Path" ?~ "/bfragprof"
    & annots . at "XLabel" ?~ "$p_{\\mathrm T}$ [GeV]"
    & annots . at "YLabel" ?~ "$<z_{p_{\\mathrm T}}>$"

sumTrkPtVsJetEtaProf :: YodaObj
sumTrkPtVsJetEtaProf = yodaProf1D 25 0 2.5
    & annots . at "Path" ?~ "/sumtrkptprof"
    & annots . at "XLabel" ?~ "$\\eta$ [GeV]"
    & annots . at "YLabel" ?~ "$<\\sum_{\\mathrm{trk}} p_{\\mathrm T}>$"

sumSVTrkPtVsJetEtaProf :: YodaObj
sumSVTrkPtVsJetEtaProf = yodaProf1D 25 0 2.5
    & annots . at "Path" ?~ "/sumsvtrkptprof"
    & annots . at "XLabel" ?~ "$\\eta$ [GeV]"
    & annots . at "YLabel" ?~ "$<\\mathrm{SV }\\sum_{\\mathrm{trk}} p_{\\mathrm T}>$"

bFragVsJetEtaProf :: YodaObj
bFragVsJetEtaProf = yodaProf1D 25 0 2.5
    & annots . at "Path" ?~ "/bfragprof"
    & annots . at "XLabel" ?~ "$\\eta$ [GeV]"
    & annots . at "YLabel" ?~ "$<z_{p_{\\mathrm T}}>$"


jetTrkObjs :: Monad m => Consumer (WithWeight Jet) m [YodaObj]
jetTrkObjs = sequenceConduits [ fillingOver (noted . _H1DD) sumTrkPtHist
                                    <=$= CL.map (fmap sumTrkPt)
                              , fillingOver (noted . _H1DD) sumSVTrkPtHist
                                    <=$= CL.map (fmap sumSVTrkPt)

                              -- TODO
                              -- this is pretty inefficient

                              , fillingOver (noted . _P1DD) sumTrkPtVsJetPtProf
                                    <=$= CL.map (\(w, j) -> (w, (lvPt (toPtEtaPhiE j), sumTrkPt j)))
                              , fillingOver (noted . _P1DD) sumSVTrkPtVsJetPtProf
                                    <=$= CL.map (\(w, j) -> (w, (lvPt (toPtEtaPhiE j), sumSVTrkPt j)))
                              , fillingOver (noted . _P1DD) sumTrkPtVsJetEtaProf
                                    <=$= CL.map (\(w, j) -> (w, (abs . lvEta $ toPtEtaPhiE j, sumTrkPt j)))
                              , fillingOver (noted . _P1DD) sumSVTrkPtVsJetEtaProf
                                    <=$= CL.map (\(w, j) -> (w, (abs . lvEta $ toPtEtaPhiE j, sumSVTrkPt j)))

                              -- make sure we don't fill this with NaNs
                              , fillAll (fillingOver (noted . _H1DD) bFragHist)
                                    <=$= CL.map (fmap bFrag)
                              , fillAll (fillingOver (noted . _P1DD) bFragVsJetPtProf)
                                    <=$= CL.map (\(w, j) -> (w, (lvPt (toPtEtaPhiE j),) <$> bFrag j))
                              , fillAll (fillingOver (noted . _P1DD) bFragVsJetEtaProf)
                                    <=$= CL.map (\(w, j) -> (w, (lvEta (toPtEtaPhiE j),) <$> bFrag j))
                              ]



type WithWeight a = (Double, a)


jetsObjs :: Monad m => Consumer (WithWeight [Jet]) m [YodaObj]
jetsObjs = fmap ((path %~ ("/jets" <>)) . (xlabel %~ ("small-$R$ jet " <>)))
             <$> (fillAll jetTrkObjs =++= fillAll lvObjs)

jet0Objs :: Monad m => Consumer (WithWeight [Jet]) m [YodaObj]
jet0Objs = fmap ((path %~ ("/jet0" <>)) . (xlabel %~ ("leading small-$R$ jet " <>)))
             <$> (fillFirst jetTrkObjs =++= fillFirst lvObjs)

probeJetObjs :: Monad m => Consumer (WithWeight [Jet]) m [YodaObj]
probeJetObjs = fillAll jetTrkObjs =++= fillAll lvObjs

allProbeJetObjs :: Monad m => Consumer (WithWeight [Jet]) m [YodaObj]
allProbeJetObjs = CL.map (fmap probeJets) =$=
    ( fmap ((path %~ ("/probejet2trk" <>)) . (xlabel %~ ("probe small-$R$ jet " <>)))
      <$> probeJetObjs <=$= CL.map (fmap (filter ((== 2) . length . jSVTracks)))
    ) =++=
    ( fmap ((path %~ ("/probejet3trk" <>)) . (xlabel %~ ("probe small-$R$ jet " <>)))
      <$> probeJetObjs <=$= CL.map (fmap (filter ((== 3) . length . jSVTracks)))
    ) =++=
    ( fmap ((path %~ ("/probejet4ptrk" <>)) . (xlabel %~ ("probe small-$R$ jet " <>)))
      <$> probeJetObjs <=$= CL.map (fmap (filter ((>= 4) . length . jSVTracks)))
    ) =++= 
    ( fmap ((path %~ ("/probejet" <>)) . (xlabel %~ ("probe small-$R$ jet " <>)))
      <$> probeJetObjs
    )


jetObjs :: Monad m => Consumer (WithWeight Event) m [YodaObj]
jetObjs = (jetsObjs =++= jet0Objs =++= allProbeJetObjs) <=$= CL.map (fmap _jets)


{-
ljetHs :: Monad m => Consumer (WithWeight LargeJet) m [YodaHist1D]
ljetHs = (fillingYH mHist <=$= CL.map (fmap ljM))
         =:= (fillingYH sd12Hist <=$= CL.map (fmap ljSD12))
         =:= lvHists


ljetsHists :: Monad m => Consumer (WithWeight LargeJets) m [YodaHist1D]
ljetsHists = fmap ((path %~ ("/ljets" <>)) . (xlabel %~ ("large-$R$ jet " <>)))
              <$> fillAll ljetHs


ljet0Hists :: Monad m => Consumer (WithWeight LargeJets) m [YodaHist1D]
ljet0Hists = fmap ((path %~ ("/ljet0" <>)) . (xlabel %~ ("leading large-$R$ jet " <>)))
              <$> fillFirst ljetHs


ljetHists :: Monad m => Consumer (WithWeight Event) m [YodaHist1D]
ljetHists = (ljetsHists =++= ljet0Hists) <=$= CL.map (fmap eLargeJets)


eljetHist :: Monad m => Consumer (WithWeight Event) m YodaHist1D
eljetHist = ((path %~ ("/elljet0" <>)) . (xlabel %~ ("electron-large-$R$ jet " <>)))
             <$> fillFirst (fillingYH dRHist) <=$= CL.map (fmap f)
    where f evt = flip minDR (eElectrons evt) =<< leading (eLargeJets evt)

tjetHists :: Monad m => Consumer (WithWeight Event) m [YodaHist1D]
tjetHists = fmap ((path %~ ("/tjets" <>)) . (xlabel %~ ("track jet " <>)))
             <$> fillAll lvHists <=$= CL.map (fmap eTrackJets)
-}

electronsObjs :: Monad m => Consumer (WithWeight Event) m [YodaObj]
electronsObjs = fmap ((path %~ ("/electrons" <>)) . (xlabel %~ ("electron " <>)))
                  <$> fillAll lvObjs <=$= CL.map (fmap _electrons)


muonsObjs :: Monad m => Consumer (WithWeight Event) m [YodaObj]
muonsObjs = fmap ((path %~ ("/muons" <>)) . (xlabel %~ ("muon " <>)))
              <$> fillAll lvObjs <=$= CL.map (fmap _muons)

metHist :: Monad m => Consumer (WithWeight Event) m YodaObj
metHist = ((path %~ ("/met" <>)) . (xlabel %~ ("$E_{\\mathrm{T}}^{\\mathrm{miss}}$ " <>)))
             <$> fillingOver (noted . _H1DD) ptHist <=$= CL.map (fmap (lvPt . _met))

eventObjs :: Monad m => Consumer (WithWeight Event) m [YodaObj]
eventObjs = {- eljetHist =:= -} metHist =:= jetObjs
                         =++= electronsObjs =++= muonsObjs
                         -- =++= ljetObjs =++= tjetObjs


channel :: Monad m => Text -> (Event -> Bool) -> Consumer (WithWeight Event) m [YodaObj]
channel n f = (fmap.fmap) (path %~ (n <>)) $ filterC (f . snd) =$= eventObjs


channelObjs :: Monad m => Consumer (WithWeight Event) m (Int, ZipList YodaObj)
channelObjs = getZipConduit $ (,) <$> ZipConduit lengthC <*> ZipConduit (ZipList . concat <$> sequenceConduits [ CL.map (fmap pruneJets) =$= channel "/elmujj/inclusive" elmujj ])
