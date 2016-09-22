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
import qualified Data.Map.Strict as M

import Data.YODA.Obj

import Data.Atlas.Event
import Data.Atlas.Selection
import Data.Atlas.Systematic

import Data.TTree


-- TODO
-- all of these lists should probably turn into (:.) lists for type checking

data Feed a b = Feed { obj :: !b
                     , feed :: a -> Feed a b
                     }

-- convert a fold to a feed
toFeed :: (a -> b -> b) -> b -> Feed a b
toFeed f o = Feed o $ \x -> let o' = f x o in toFeed f o'

premap :: (a -> c) -> Feed c b -> Feed a b
premap f (Feed o g) = Feed o $ \x -> premap f (g (f x))

instance Functor (Feed a) where
    fmap g (Feed x f) = Feed (g x) $ \x' -> fmap g (f x')

instance Applicative (Feed a) where
    pure x = let f = Feed x (const f) in f
    Feed g f <*> Feed x f' = Feed (g x) $ \x' -> f x' <*> f' x'


-- need monad?

-- fixme
feedOver :: Traversal' b c -> (a -> c -> c) -> b -> Feed a b
feedOver l f = toFeed $ over l . f

feedAll :: Foldable f => Feed a b -> Feed (f a) b
feedAll f@(Feed o g) = Feed o $ feedAll . foldl feed f

feedFirst :: Foldable f => Feed a b -> Feed (f a) b
feedFirst f = premap (listToMaybe . toList) (feedAll f)

feedIf :: (a -> Bool) -> Feed a b -> Feed a b
feedIf g f = premap g' $ feedAll f
    where g' x = if g x then Just x else Nothing

fillOver :: Fillable a => Traversal' b a -> b -> Feed (FillVec a) b
fillOver l = feedOver l fill


-- not sure about these fixities.
infixr 2 <$=
(<$=) :: Feed c b -> (a -> c) -> Feed a b
(<$=) = flip premap


infixr 3 =:=
(=:=) :: Applicative f => f a -> f [a] -> f [a]
fx =:= fxs = (:) <$> fx <*> fxs


infixr 3 =++=
(=++=) :: Applicative f => f [a] -> f [a] -> f [a]
fxs =++= fys = (++) <$> fxs <*> fys


type ObjsFiller a = Feed (WithWeight a) [YodaObj]


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

lvObjs :: HasLorentzVector a => ObjsFiller a
lvObjs = sequenceA [ fillOver (noted . _H1DD) ptHist <$= fmap (view lvPt)
                   , fillOver (noted . _H1DD) etaHist <$= fmap (view lvEta)
                   ] <$= fmap (view toPtEtaPhiE)



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
                       , feedAll (fillOver (noted . _H1DD) bFragHist)
                             <$= sequence . fmap bFrag
                       , feedAll (fillOver (noted . _P1DD) bFragVsJetPtProf)
                             <$= (\(w, j) -> sequence (w, (view lvPt j,) <$> bFrag j))
                       , feedAll (fillOver (noted . _P1DD) bFragVsJetEtaProf)
                             <$= (\(w, j) -> sequence (w, (view lvEta j,) <$> bFrag j))

                       , feedAll (fillOver (noted . _P1DD) bFragVsTrkSumPtProf)
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


channel :: Text -> (a -> Bool) -> ObjsFiller a -> ObjsFiller a
channel n f c = feedIf (fmap f) $ fmap (path %~ (n <>)) <$> c


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
jetObjs = feedAll jetsObjs =++= feedFirst jet0Objs


electronsObjs :: ObjsFiller (Event a)
electronsObjs = fmap ((path %~ ("/electrons" <>)) . (xlabel %~ ("electron " <>)))
                  <$> feedAll lvObjs <$= fmap _electrons


{-
muonsObjs :: ObjsFiller (Event a)
muonsObjs = fmap ((path %~ ("/muons" <>)) . (xlabel %~ ("muon " <>)))
              <$> feedAll lvObjs <$= CL.map (fmap _muons)

metHist :: HistFiller (Event a)
metHist = ((path %~ ("/met" <>)) . (xlabel %~ ("$E_{\\mathrm{T}}^{\\mathrm{miss}}$ " <>)))
             <$> fillingOver (noted . _H1DD) ptHist <$= CL.map (fmap (view $ met . lvPt))


{-
-- TODO
-- we only want to pass the list of event weights once.
-- need to consolidate a bit.
mcEventObjs :: MonadIO m => [WeightSystematic] -> TTree -> m [YodaObj]
mcEventObjs ws t = do
    hMap <- runTTree (readEventSysts ws) t $$ fillHists
    return . concatMap (\(n, hs) -> fmap (path %~ (n <>)) hs) . M.toList $ hMap

    where
        -- hists :: Monad m => Consumer (WithWeight (Event MC)) m [YodaObj]
        -- hists = metHist =:= electronsObjs =++= muonsObjs
                    -- =++= (CL.map (fmap (view jets))
                        -- =$= (jetObjs =++= (CL.map (fmap probeJets) =$= feedAll probeJetMCObjs)))

-}

fillHists :: Monad m
          => [Text] -> Consumer (Event MC) m [YodaObj] -> Consumer (M.Map Text (Event MC)) m (M.Map Text [YodaObj])
fillHists ns c = go mcs

    where
        -- mcs :: Monad m => M.Map Text (ConduitM (Event MC) o m [YodaObj])
        mcs = M.fromList $ map (,c) ns

        go :: M.Map Text (ConduitM a o m b) -> ConduitM (M.Map Text a) o m (M.Map Text b)
        go myos = do
            mmap <- await
            case mmap of
                Nothing -> do xs <- runConduit $ sequence myos
                              return xs
                Just m -> go $ M.intersectionWith (fuse . yield) m myos

         

{-
fillHists mcs = do mevts <- await
                   case mevts of
                        Nothing -> sequence mcs
                        Just e -> sequence mcs

-}
-- fillHists mcs = foldlC (intersectionWith (flip $ fuse .  yield)) $ M.fromList (map (\w -> (weightName w, hists)) ws)

dataEventObjs :: ObjsFiller (Event Data')
dataEventObjs = metHist =:= electronsObjs =++= muonsObjs
    =++= (CL.map (fmap (view jets))
        =$= (jetObjs
            =++= (CL.map (fmap probeJets) =$= feedAll probeJetDataObjs))
         )



withLenC :: Monad m => ConduitM i o m a -> ConduitM i o m (Int, a)
withLenC c =
    getZipConduit $
        (,) <$> ZipConduit lengthC
            <*> ZipConduit c


dataEvent :: Monad m => Conduit (Event Data') m (WithWeight (Event Data'))
dataEvent = CL.map (1.0,)
-}
