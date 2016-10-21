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
feed (F.Fold f o g) x = let o' = f o x in o' `seq` F.Fold f o' g

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



type SystFiller a = F.Fold (M.Map Text Double, a) [M.Map Text YodaObj]

liftSyst :: F.Fold (w, x) YodaObj -> F.Fold (M.Map Text w, x) (M.Map Text YodaObj)
liftSyst (F.Fold f o g) = F.Fold f' M.empty (fmap g)
    where
        f' hs (mw, x) = M.mergeWithKey (\_ a b -> Just $ f a (b, x)) id (fmap $ f o . (,x)) hs mw

fillHistSyst :: YodaObj -> F.Fold (M.Map Text Double, Double) (M.Map Text YodaObj)
fillHistSyst h = liftSyst $ fillOver (noted . _H1DD) h

fillProfSyst :: YodaObj -> F.Fold (M.Map Text Double, (Double, Double)) (M.Map Text YodaObj)
fillProfSyst p = liftSyst $ fillOver (noted . _P1DD) p




lvObjs :: HasLorentzVector a => SystFiller a
lvObjs = sequenceA [ fillHistSyst ptHist <$= fmap (view lvPt)
                   , fillHistSyst etaHist <$= fmap (view lvEta)
                   ] <$= fmap (view toPtEtaPhiE)


jetTrkObjs :: SystFiller (Jet a)
jetTrkObjs = 
    sequenceA [ fillHistSyst trkSumPtHist
                    <$= fmap (view (trkSum.lvPt))
              , fillHistSyst svTrkSumPtHist
                    <$= fmap (view (svTrkSum.lvPt))
              , fillHistSyst mv2c10Hist
                    <$= fmap (view mv2c10)

              -- TODO
              -- this is pretty (no---*really*) inefficient

              , fillProfSyst trkSumPtVsJetPtProf
                    <$= (\(w, j) -> (w, (view lvPt j, view (trkSum.lvPt) j)))
              , fillProfSyst svTrkSumPtVsJetPtProf
                    <$= (\(w, j) -> (w, (view lvPt j, view (svTrkSum.lvPt) j)))
              , fillProfSyst trkSumPtVsJetEtaProf
                    <$= (\(w, j) -> (w, (view lvAbsEta j, view (trkSum.lvPt) j)))
              , fillProfSyst svTrkSumPtVsJetEtaProf
                    <$= (\(w, j) -> (w, (view lvAbsEta j, view (svTrkSum.lvPt) j)))

              , fillProfSyst svTrkSumPtVsTrkSumPtProf
                    <$= (\(w, j) -> (w, (view (trkSum.lvPt) j, view (svTrkSum.lvPt) j)))

              -- make sure we don't fill this with NaNs
              , foldAll (fillHistSyst bFragHist)
                    <$= sequence . fmap (view bFrag)
              , foldAll (fillProfSyst bFragVsJetPtProf)
                    <$= (\(w, j) -> sequence (w, (view lvPt j,) <$> view bFrag j))
              , foldAll (fillProfSyst bFragVsJetEtaProf)
                    <$= (\(w, j) -> sequence (w, (view lvEta j,) <$> view bFrag j))

              , foldAll (fillProfSyst bFragVsTrkSumPtProf)
                    <$= (\(w, j) -> sequence (w, (view (trkSum.lvPt) j,) <$> view bFrag j))

              , fillHistSyst nPVTrksHist
                    <$= fmap (fromIntegral . length . view pvTracks)
              , fillHistSyst nSVTrksHist
                    <$= fmap (fromIntegral . length . view svTracks)
              , fillProfSyst nPVTrksVsJetPtProf
                    <$= fmap ((,) <$> view lvPt <*> fromIntegral . length . view pvTracks)
              , fillProfSyst nSVTrksVsJetPtProf
                    <$= fmap ((,) <$> view lvPt <*> fromIntegral . length . view svTracks)
              ]



jetsObjs :: SystFiller (Jet a)
jetsObjs = (fmap.fmap) ((path %~ ("/jets" <>)) . (xlabel %~ ("small-$R$ jet " <>)))
             <$> lvObjs


jet0Objs :: SystFiller (Jet a)
jet0Objs = (fmap.fmap) ((path %~ ("/jet0" <>)) . (xlabel %~ ("leading small-$R$ jet " <>)))
             <$> lvObjs


probeJetObjs :: SystFiller (Jet a)
probeJetObjs = jetTrkObjs =++= lvObjs


channel :: Text -> (a -> Bool) -> SystFiller a -> SystFiller a
channel n f c = foldIf (f.snd) $ over (traverse.traverse.path) (n <>) <$> c


{-
 - TODO
allProbeJetObjs :: SystFiller (Jet a)
allProbeJetObjs = sets (traverse.traverse.xlabel) ("probe small-$R$ jet " <>) $
    channel "/probejet2trk" ((== 2) . nSVTracks . snd) probeJetObjs
    =++= channel "/probejet3trk" ((== 3) . nSVTracks . snd) probeJetObjs
    =++= channel "/probejet4ptrk" ((>= 4) . nSVTracks . snd) probeJetObjs
    =++= channel "/probejet" (const True) probeJetObjs
    

probeJetMCObjs :: SystFiller (Jet MC)
probeJetMCObjs =
    channel "/bjets" (bLabeled . snd) allProbeJetObjs
    =++= channel "/cjets" (cLabeled . snd) allProbeJetObjs
    =++= channel "/ljets" (lLabeled . snd) allProbeJetObjs
    =++= channel "" (const True) allProbeJetObjs
-}

allProbeJetObjs :: SystFiller (Jet a)
allProbeJetObjs = over (traverse.traverse.xlabel) ("probe small-$R$ jet " <>) <$> probeJetObjs


probeJetMCObjs :: SystFiller (Jet MC)
probeJetMCObjs = allProbeJetObjs

probeJetDataObjs :: SystFiller (Jet Data')
probeJetDataObjs = allProbeJetObjs


jetObjs :: SystFiller [Jet a]
jetObjs = foldAll jetsObjs =++= foldFirst jet0Objs <$= sequence


electronsObjs :: SystFiller (Event a)
electronsObjs = (fmap.fmap) ((path %~ ("/electrons" <>)) . (xlabel %~ ("electron " <>)))
                  <$> foldAll lvObjs <$= sequence . fmap _electrons


muonsObjs :: SystFiller (Event a)
muonsObjs = (fmap.fmap) ((path %~ ("/muons" <>)) . (xlabel %~ ("muon " <>)))
              <$> foldAll lvObjs <$= sequence . fmap _muons

metObj :: F.Fold (M.Map Text Double, Event a) (M.Map Text YodaObj)
metObj = fmap ((path %~ ("/met" <>)) . (xlabel %~ ("$E_{\\mathrm{T}}^{\\mathrm{miss}}$ " <>)))
             <$> fillHistSyst ptHist <$= fmap (view $ met . lvPt)

muObj :: F.Fold (M.Map Text Double, Event a) (M.Map Text YodaObj)
muObj = fillHistSyst muHist <$= fmap (view mu)


{-
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



dataEventObjs :: SystFiller (Event Data')
dataEventObjs = muObj =:= metObj =:= electronsObjs =++= muonsObjs
    =++= F.premap (fmap (view jets))
            (jetObjs =++= (foldAll probeJetDataObjs <$= sequence . fmap probeJets))
-}

eventObjs :: SystFiller (Event a)
eventObjs = muObj =:= metObj =:= electronsObjs =++= muonsObjs
    =++= F.premap (fmap (view jets))
            (jetObjs =++= (foldAll allProbeJetObjs <$= sequence . fmap probeJets))


lengthF :: F.Fold a Int
lengthF = toFold (flip $ const (+1)) 0

withLenF :: F.Fold a b -> F.Fold a (Int, b)
withLenF f = (\x y -> x `seq` y `seq` (x, y)) <$> lengthF <*> f

dataEvent :: Event Data' -> (M.Map Text Double, Event Data')
dataEvent = (M.singleton "nominal" 1.0,)
