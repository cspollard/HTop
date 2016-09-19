{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Atlas.Histograms where

import Control.Lens

import Conduit
import qualified Data.Conduit.List as CL

import Data.Maybe (listToMaybe)
import Data.Foldable (toList)

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

fillFirst :: (Monad m, Foldable f)
          => Consumer (v, a) m b -> Consumer (v, f a) m b
fillFirst c = CL.map (traverse firstF) =$= CL.concat =$= c

firstF :: Foldable f => f a -> Maybe a
firstF = listToMaybe . toList


-- not sure about these fixities.
infixr 2 <=$=
(<=$=) :: Monad m => ConduitM b c m r -> Conduit a m b -> ConduitM a c m r
(<=$=) = flip (=$=)


infixr 3 =:=
(=:=) :: Monad m => ConduitM i o m r -> ConduitM i o m [r] -> ConduitM i o m [r]
c =:= cs = getZipConduit $ (:) <$> ZipConduit c <*> ZipConduit cs


infixr 3 =++=
(=++=) :: Monad m => ConduitM i o m [r] -> ConduitM i o m [r] -> ConduitM i o m [r]
cs =++= cs' = getZipConduit $ (++) <$> ZipConduit cs <*> ZipConduit cs'


dsigdXpbY :: Text -> Text -> Text
dsigdXpbY x y = "$\\frac{d\\sigma}{d" <> x <> "} \\frac{\\mathrm{pb}}{" <> y <> "}$"

gev, rad, pt :: Text
gev = "\\mathrm{GeV}"
rad = "\\mathrm{rad}"
pt = "p_{\\mathrm{T}}"


type WithWeight a = (Double, a)

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

lvObjs :: (Monad m, HasLorentzVector a) => Consumer (WithWeight a) m [YodaObj]
lvObjs = sequenceConduits [ fillingOver (noted . _H1DD) ptHist  <=$= CL.map (fmap $ view lvPt)
                          , fillingOver (noted . _H1DD) etaHist <=$= CL.map (fmap $ view lvEta)
                          ] <=$= CL.map (fmap $ view toPtEtaPhiE)



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


jetTrkObjs :: Monad m => Consumer (WithWeight (Jet a)) m [YodaObj]
jetTrkObjs = sequenceConduits [ fillingOver (noted . _H1DD) trkSumPtHist
                                    <=$= CL.map (fmap trkSumPt)
                              , fillingOver (noted . _H1DD) svTrkSumPtHist
                                    <=$= CL.map (fmap svTrkSumPt)

                              -- TODO
                              -- this is pretty (no---*really*) inefficient

                              , fillingOver (noted . _P1DD) trkSumPtVsJetPtProf
                                    <=$= CL.map (\(w, j) -> (w, (view lvPt j, trkSumPt j)))
                              , fillingOver (noted . _P1DD) svTrkSumPtVsJetPtProf
                                    <=$= CL.map (\(w, j) -> (w, (view lvPt j, svTrkSumPt j)))
                              , fillingOver (noted . _P1DD) trkSumPtVsJetEtaProf
                                    <=$= CL.map (\(w, j) -> (w, (view lvAbsEta j, trkSumPt j)))
                              , fillingOver (noted . _P1DD) svTrkSumPtVsJetEtaProf
                                    <=$= CL.map (\(w, j) -> (w, (view lvAbsEta j, svTrkSumPt j)))

                              , fillingOver (noted . _P1DD) svTrkSumPtVsTrkSumPtProf
                                    <=$= CL.map (\(w, j) -> (w, (trkSumPt j, svTrkSumPt j)))

                              -- make sure we don't fill this with NaNs
                              , fillAll (fillingOver (noted . _H1DD) bFragHist)
                                    <=$= CL.map (fmap bFrag)
                              , fillAll (fillingOver (noted . _P1DD) bFragVsJetPtProf)
                                    <=$= CL.map (\(w, j) -> (w, (view lvPt j,) <$> bFrag j))
                              , fillAll (fillingOver (noted . _P1DD) bFragVsJetEtaProf)
                                    <=$= CL.map (\(w, j) -> (w, (view lvEta j,) <$> bFrag j))

                              , fillAll (fillingOver (noted . _P1DD) bFragVsTrkSumPtProf)
                                    <=$= CL.map (\(w, j) -> (w, (trkSumPt j,) <$> bFrag j))

                              , fillingOver (noted . _H1DD) nPVTrksHist
                                    <=$= CL.map (fmap (fromIntegral . length . view jPVTracks))
                              , fillingOver (noted . _H1DD) nSVTrksHist
                                    <=$= CL.map (fmap (fromIntegral . length . view jSVTracks))
                              , fillingOver (noted . _P1DD) nPVTrksVsJetPtProf
                                    <=$= CL.map (fmap ((,) <$> view lvPt <*> fromIntegral . length . view jPVTracks))
                              , fillingOver (noted . _P1DD) nSVTrksVsJetPtProf
                                    <=$= CL.map (fmap ((,) <$> view lvPt <*> fromIntegral . length . view jSVTracks))
                              ]



jetsObjs :: Monad m => Consumer (WithWeight (Jet a)) m [YodaObj]
jetsObjs = fmap ((path %~ ("/jets" <>)) . (xlabel %~ ("small-$R$ jet " <>)))
             <$> lvObjs


jet0Objs :: Monad m => Consumer (WithWeight (Jet a)) m [YodaObj]
jet0Objs = fmap ((path %~ ("/jet0" <>)) . (xlabel %~ ("leading small-$R$ jet " <>)))
             <$> lvObjs


probeJetObjs :: Monad m => Consumer (WithWeight (Jet a)) m [YodaObj]
probeJetObjs = jetTrkObjs =++= lvObjs



allProbeJetObjs :: Monad m => Consumer (WithWeight (Jet a)) m [YodaObj]
allProbeJetObjs = (fmap.fmap) (xlabel %~ ("probe small-$R$ jet " <>)) $
    channel "/probejet2trk" ((== 2) . nSVTracks . snd) probeJetObjs
    =++= channel "/probejet3trk" ((== 3) . nSVTracks . snd) probeJetObjs
    =++= channel "/probejet4ptrk" ((>= 4) . nSVTracks . snd) probeJetObjs
    =++= channel "/probejet" (const True) probeJetObjs
    

probeJetMCObjs :: Monad m => Consumer (WithWeight (Jet (MC' a))) m [YodaObj]
probeJetMCObjs =
    channel "/bjets" (bLabeled . snd) allProbeJetObjs
    =++= channel "/cjets" (cLabeled . snd) allProbeJetObjs
    =++= channel "/ljets" (lLabeled . snd) allProbeJetObjs
    =++= channel "" (const True) allProbeJetObjs


probeJetDataObjs :: Monad m => Consumer (WithWeight (Jet Data')) m [YodaObj]
probeJetDataObjs = allProbeJetObjs


jetObjs :: Monad m => Consumer (WithWeight [Jet a]) m [YodaObj]
jetObjs = fillAll jetsObjs
            =++= fillFirst jet0Objs


electronsObjs :: Monad m => Consumer (WithWeight (Event a)) m [YodaObj]
electronsObjs = fmap ((path %~ ("/electrons" <>)) . (xlabel %~ ("electron " <>)))
                  <$> fillAll lvObjs <=$= CL.map (fmap _electrons)


muonsObjs :: Monad m => Consumer (WithWeight (Event a)) m [YodaObj]
muonsObjs = fmap ((path %~ ("/muons" <>)) . (xlabel %~ ("muon " <>)))
              <$> fillAll lvObjs <=$= CL.map (fmap _muons)

metHist :: Monad m => Consumer (WithWeight (Event a)) m YodaObj
metHist = ((path %~ ("/met" <>)) . (xlabel %~ ("$E_{\\mathrm{T}}^{\\mathrm{miss}}$ " <>)))
             <$> fillingOver (noted . _H1DD) ptHist <=$= CL.map (fmap (view $ met . lvPt))

mcEventObjs :: Monad m => Consumer (WithWeight (Event (MC' a))) m [YodaObj]
mcEventObjs = metHist =:= electronsObjs =++= muonsObjs
    =++= (CL.map (fmap (view jets))
        =$= (jetObjs
            =++= (CL.map (fmap probeJets) =$= fillAll probeJetMCObjs))
         )

dataEventObjs :: Monad m => Consumer (WithWeight (Event Data')) m [YodaObj]
dataEventObjs = metHist =:= electronsObjs =++= muonsObjs
    =++= (CL.map (fmap (view jets))
        =$= (jetObjs
            =++= (CL.map (fmap probeJets) =$= fillAll probeJetDataObjs))
         )


channel :: Monad m =>
    Text -> (a -> Bool)
    -> Consumer a m [YodaObj]
    -> Consumer a m [YodaObj]
channel n f c = filterC f =$= fmap (path %~ (n <>)) <$> c


withLenC :: Monad m => ConduitM i o m a -> ConduitM i o m (Int, a)
withLenC c =
    getZipConduit $
        (,) <$> ZipConduit lengthC
            <*> ZipConduit c


mcEvent :: Monad m => Conduit (Event (MC' a)) m (WithWeight (Event (MC' a)))
mcEvent = CL.map ((,) <$> view extraInfo <*> id)

dataEvent :: Monad m => Conduit (Event Data') m (WithWeight (Event Data'))
dataEvent = CL.map (1.0,)

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
