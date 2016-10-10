{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Atlas.Histograms where

import Control.Lens
import qualified Control.Foldl as F

import Data.Maybe (listToMaybe)
import Data.Foldable (toList)
import Data.Semigroup

import Data.Text (Text)
import qualified Data.Map.Strict as M

import Data.YODA.Obj

import Data.Atlas.Event
import Data.Atlas.Selection


feed :: (b -> a -> b) -> b -> F.Fold a b
feed f o = F.Fold f o id

lseq :: [a] -> [a]
lseq [] = []
lseq (x:xs) = seq x $ x : lseq xs


-- fixme
foldOver :: Traversal' b c -> (c -> a -> c) -> b -> (b -> b') -> F.Fold a b'
foldOver l f = F.Fold (\y x -> over l (`f` x) y)

foldAll :: F.Foldable f => F.Fold a b -> F.Fold (f a) b
foldAll (F.Fold f o g) = F.Fold (foldl f) o g

foldFirst :: F.Foldable f => F.Fold a b -> F.Fold (f a) b
foldFirst f = F.premap (listToMaybe . toList) (foldAll f)

foldIf :: (a -> Bool) -> F.Fold a b -> F.Fold a b
foldIf g f = F.premap g' $ foldAll f
    where g' x = if g x then Just x else Nothing

fillOver :: Fillable a => Traversal' b a -> b -> F.Fold (FillVec a) b
fillOver l o = foldOver l (flip fill) o id


-- not sure about these fixities.
infixl 2 <$=
(<$=) :: F.Fold c b -> (a -> c) -> F.Fold a b
(<$=) = flip F.premap


infixr 3 =:=
(=:=) :: Applicative f => f a -> f [a] -> f [a]
fx =:= fxs = (:) <$> fx <*> fxs


infixr 3 =++=
(=++=) :: Applicative f => f [a] -> f [a] -> f [a]
fxs =++= fys = (++) <$> fxs <*> fys


type ObjsFiller a = F.Fold (WithWeight a) [YodaObj]


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

lvObjs :: HasLorentzVector a => ObjsFiller a
lvObjs = sequenceA [ fillOver (noted . _H1DD) ptHist <$= fmap (view lvPt)
                   , fillOver (noted . _H1DD) etaHist <$= fmap (view lvEta)
                   ] <$= fmap (view toPtEtaPhiE)



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


jetTrkObjs :: ObjsFiller (Jet a)
jetTrkObjs = sequenceA [ fillOver (noted . _H1DD) trkSumPtHist
                             <$= fmap trkSumPt
                       , fillOver (noted . _H1DD) svTrkSumPtHist
                             <$= fmap svTrkSumPt
                       , fillOver (noted . _H1DD) mv2c10Hist
                             <$= fmap (view jMV2c10)

                       -- TODO
                       -- this is pretty (no---*really*) inefficient

                       , fillOver (noted . _P1DD) trkSumPtVsJetPtProf
                             <$= (\(w, j) -> (w, (view lvPt j, trkSumPt j)))
                       , fillOver (noted . _P1DD) svTrkSumPtVsJetPtProf
                             <$= (\(w, j) -> (w, (view lvPt j, svTrkSumPt j)))
                       , fillOver (noted . _P1DD) trkSumPtVsJetEtaProf
                             <$= (\(w, j) -> (w, (view lvAbsEta j, trkSumPt j)))
                       , fillOver (noted . _P1DD) svTrkSumPtVsJetEtaProf
                             <$= (\(w, j) -> (w, (view lvAbsEta j, svTrkSumPt j)))

                       , fillOver (noted . _P1DD) svTrkSumPtVsTrkSumPtProf
                             <$= (\(w, j) -> (w, (trkSumPt j, svTrkSumPt j)))

                       -- make sure we don't fill this with NaNs
                       , foldAll (fillOver (noted . _H1DD) bFragHist)
                             <$= sequence . fmap bFrag
                       , foldAll (fillOver (noted . _P1DD) bFragVsJetPtProf)
                             <$= (\(w, j) -> sequence (w, (view lvPt j,) <$> bFrag j))
                       , foldAll (fillOver (noted . _P1DD) bFragVsJetEtaProf)
                             <$= (\(w, j) -> sequence (w, (view lvEta j,) <$> bFrag j))

                       , foldAll (fillOver (noted . _P1DD) bFragVsTrkSumPtProf)
                             <$= (\(w, j) -> sequence (w, (trkSumPt j,) <$> bFrag j))

                       , fillOver (noted . _H1DD) nPVTrksHist
                             <$= fmap (fromIntegral . length . view jPVTracks)
                       , fillOver (noted . _H1DD) nSVTrksHist
                             <$= fmap (fromIntegral . length . view jSVTracks)
                       , fillOver (noted . _P1DD) nPVTrksVsJetPtProf
                             <$= fmap ((,) <$> view lvPt <*> fromIntegral . length . view jPVTracks)
                       , fillOver (noted . _P1DD) nSVTrksVsJetPtProf
                             <$= fmap ((,) <$> view lvPt <*> fromIntegral . length . view jSVTracks)
                       ]



jetsObjs :: ObjsFiller (Jet a)
jetsObjs = fmap ((path %~ ("/jets" <>)) . (xlabel %~ ("small-$R$ jet " <>)))
             <$> lvObjs


jet0Objs :: ObjsFiller (Jet a)
jet0Objs = fmap ((path %~ ("/jet0" <>)) . (xlabel %~ ("leading small-$R$ jet " <>)))
             <$> lvObjs


probeJetObjs :: ObjsFiller (Jet a)
probeJetObjs = jetTrkObjs =++= lvObjs


channel :: Functor f
        => Text -> (a -> Bool) -> F.Fold a (f YodaObj) -> F.Fold a (f YodaObj)
channel n f c = foldIf f $ fmap (path %~ (n <>)) <$> c


allProbeJetObjs :: ObjsFiller (Jet a)
allProbeJetObjs = (fmap.fmap) (xlabel %~ ("probe small-$R$ jet " <>)) $
    channel "/probejet2trk" ((== 2) . nSVTracks . snd) probeJetObjs
    =++= channel "/probejet3trk" ((== 3) . nSVTracks . snd) probeJetObjs
    =++= channel "/probejet4ptrk" ((>= 4) . nSVTracks . snd) probeJetObjs
    =++= channel "/probejet" (const True) probeJetObjs
    

probeJetMCObjs :: ObjsFiller (Jet MC)
probeJetMCObjs =
    channel "/bjets" (bLabeled . snd) allProbeJetObjs
    =++= channel "/cjets" (cLabeled . snd) allProbeJetObjs
    =++= channel "/ljets" (lLabeled . snd) allProbeJetObjs
    =++= channel "" (const True) allProbeJetObjs


probeJetDataObjs :: ObjsFiller (Jet Data')
probeJetDataObjs = allProbeJetObjs


jetObjs :: ObjsFiller [Jet a]
jetObjs = foldAll jetsObjs =++= foldFirst jet0Objs <$= sequence


electronsObjs :: ObjsFiller (Event a)
electronsObjs = fmap ((path %~ ("/electrons" <>)) . (xlabel %~ ("electron " <>)))
                  <$> foldAll lvObjs <$= sequence . fmap _electrons


muonsObjs :: ObjsFiller (Event a)
muonsObjs = fmap ((path %~ ("/muons" <>)) . (xlabel %~ ("muon " <>)))
              <$> foldAll lvObjs <$= sequence . fmap _muons

metObj :: F.Fold (WithWeight (Event a)) YodaObj
metObj = ((path %~ ("/met" <>)) . (xlabel %~ ("$E_{\\mathrm{T}}^{\\mathrm{miss}}$ " <>)))
             <$> fillOver (noted . _H1DD) ptHist <$= fmap (view $ met . lvPt)

muObj :: F.Fold (WithWeight (Event a)) YodaObj
muObj = fillOver (noted . _H1DD) muHist <$= fmap (view mu)


mcEventObjs :: [WeightSystematic] -> F.Fold (M.Map Text (Event MC)) [YodaObj]
mcEventObjs ws = allHists

    where
        mcHists :: F.Fold (WithWeight (Event MC)) [YodaObj]
        mcHists = muObj =:= metObj =:= electronsObjs =++= muonsObjs
                =++= F.premap (fmap (view jets))
                    (jetObjs =++= (foldAll probeJetMCObjs <$= sequence . fmap probeJets))

        allHists = fmap concat . sequenceA $ fmap f ws

        f :: WeightSystematic -> F.Fold (M.Map Text (Event MC)) [YodaObj]
        f w = let n = systName w
              in  fmap (path %~ (<> "[" <> n <> "]")) <$>
                    mcHists <$= (\e -> (view mcInfo e, e)) . (M.! n)



dataEventObjs :: ObjsFiller (Event Data')
dataEventObjs = muObj =:= metObj =:= electronsObjs =++= muonsObjs
    =++= F.premap (fmap (view jets))
            (jetObjs =++= (foldAll probeJetDataObjs <$= sequence . fmap probeJets))


lengthF :: F.Fold a Int
lengthF = feed (flip $ const (+1)) 0

withLenF :: F.Fold a b -> F.Fold a (Int, b)
withLenF f = (\x y -> x `seq` y `seq` (x, y)) <$> lengthF <*> f

dataEvent :: Event Data' -> WithWeight (Event Data')
dataEvent = (1.0,)
