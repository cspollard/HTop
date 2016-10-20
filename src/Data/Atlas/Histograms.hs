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
import Data.Atlas.HistDefs


toFold :: (b -> a -> b) -> b -> F.Fold a b
toFold f o = F.Fold f o id

feed :: F.Fold a b -> a -> F.Fold a b
feed (F.Fold f o g) x = F.Fold f (f o x) g

lseq :: [a] -> [a]
lseq [] = []
lseq (x:xs) = seq x $ x : lseq xs


foldAll :: F.Foldable f => F.Fold a b -> F.Fold (f a) b
foldAll = F.handles folded

foldFirst :: F.Foldable f => F.Fold a b -> F.Fold (f a) b
foldFirst f = F.premap (listToMaybe . toList) (foldAll f)

foldIf :: (a -> Bool) -> F.Fold a b -> F.Fold a b
foldIf g f = F.premap g' $ foldAll f
    where g' x = if g x then Just x else Nothing

fillOver :: Fillable a => Traversal' b a -> b -> F.Fold (Weight a, FillVec a) b
fillOver l = toFold (\y (w, x) -> over l (filling w x) y)

fillHist :: YodaObj -> (b -> Double) -> F.Fold (Double, b) YodaObj
fillHist h f = fillOver (noted . _H1DD) h <$= fmap f

fillProf :: YodaObj -> (b -> (Double, Double)) -> F.Fold (Double, b) YodaObj
fillProf p f = fillOver (noted . _P1DD) p <$= fmap f

liftSysts :: F.Fold (Double, a) YodaObj -> F.Fold (M.Map Text Double, a) (M.Map Text YodaObj)
liftSysts (F.Fold f o g) = F.Fold f' M.empty (fmap g)
    where
        f' hs (mw, x) = M.mergeWithKey (\_ a b -> Just $ f a (b, x)) id (fmap $ f o . (,x)) hs mw


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




type WithWeight a = (Double, a)

type ObjsFiller a = F.Fold (WithWeight a) [YodaObj]

lvObjs :: HasLorentzVector a => ObjsFiller a
lvObjs = sequenceA [ fillOver (noted . _H1DD) ptHist <$= fmap (view lvPt)
                   , fillOver (noted . _H1DD) etaHist <$= fmap (view lvEta)
                   ] <$= fmap (view toPtEtaPhiE)


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
        mcHists = muObj =:= metObj =:= electronsObjs =++= muonsObjs
                =++= F.premap (fmap (view jets))
                    (jetObjs =++= (foldAll probeJetMCObjs <$= sequence . fmap probeJets))

        allHists = fmap concat . sequenceA $ fmap f ws

        f w = let n = systName w
              in  fmap (path %~ (<> "[" <> n <> "]")) <$>
                    mcHists <$= (\e -> (view mcInfo e, e)) . (M.! n)



dataEventObjs :: ObjsFiller (Event Data')
dataEventObjs = muObj =:= metObj =:= electronsObjs =++= muonsObjs
    =++= F.premap (fmap (view jets))
            (jetObjs =++= (foldAll probeJetDataObjs <$= sequence . fmap probeJets))


lengthF :: F.Fold a Int
lengthF = toFold (flip $ const (+1)) 0

withLenF :: F.Fold a b -> F.Fold a (Int, b)
withLenF f = (\x y -> x `seq` y `seq` (x, y)) <$> lengthF <*> f

dataEvent :: Event Data' -> WithWeight (Event Data')
dataEvent = (1.0,)
