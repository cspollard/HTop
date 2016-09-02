{-# LANGUAGE OverloadedStrings, TupleSections, TypeOperators, TypeFamilies, RankNTypes #-}

module Data.Atlas.Histograms where

import Control.Lens

import Conduit
import qualified Data.Conduit.List as CL

import qualified Data.Vector.Unboxed as V

import Data.Maybe (listToMaybe)
import Data.Foldable (toList)

import Control.Applicative (liftA2, ZipList(..))

import Data.Semigroup

import Data.Text (Text)

import Data.Histogram hiding (sum)
import Data.Histogram.Extra

import Data.Atlas.Event
import Data.Atlas.Selection


-- TODO
-- all of these lists should probably turn into (:.) lists for type checking

type Weighted a = (Double, a)

dsigdXpbY :: Text -> Text -> Text
dsigdXpbY x y = "$\\frac{d\\sigma}{d" <> x <> "} \\frac{\\mathrm{pb}}{" <> y <> "}$"

gev, rad, pt :: Text
gev = "\\mathrm{GeV}"
rad = "\\mathrm{rad}"
pt = "p_{\\mathrm{T}}"

histo1D :: (V.Unbox v, Num v, Bin b) => b -> Histogram b v
histo1D bin = histogram bin (V.replicate (nBins bin) 0)


ptHisto :: YodaHisto1D
ptHisto = YodaHisto "/pt" "$p_{\\mathrm T}$ [GeV]" (dsigdXpbY pt gev) $ histo1D (binD 0 25 1000)

sumTrkPtHisto :: YodaHisto1D
sumTrkPtHisto = YodaHisto "/sumtrkpt" "$\\sum_{\\mathrm{trk}} p_{\\mathrm T}$ [GeV]" (dsigdXpbY pt gev) $ histo1D (binD 0 25 500)

sumSV1TrkPtHisto :: YodaHisto1D
sumSV1TrkPtHisto = YodaHisto "/sumsv1trkpt" "SV1 $\\sum_{\\mathrm{trk}} p_{\\mathrm T}$ [GeV]" (dsigdXpbY pt gev) $ histo1D (binD 0 25 500)

bFragHisto :: YodaHisto1D
bFragHisto = YodaHisto "/bFrag" "$z_{p_{\\mathrm T}}$" (dsigdXpbY "$z_{p_{\\mathrm T}}$" "1") $ histo1D (binD 0 22 1.1)

eHisto :: YodaHisto1D
eHisto = YodaHisto "/E" "$E$ [GeV]" (dsigdXpbY "E" gev) $ histo1D (binD 0 25 1000)

mHisto :: YodaHisto1D
mHisto = YodaHisto "/mass" "mass [GeV]" (dsigdXpbY "m" gev) $ histo1D (binD 0 30 300)

etaHisto :: YodaHisto1D
etaHisto = YodaHisto "/eta" "$\\eta$" (dsigdXpbY "\\eta" rad) $ histo1D (binD (-3) 30 3)

phiHisto :: YodaHisto1D
phiHisto = YodaHisto "/phi" "$\\phi$" (dsigdXpbY "\\phi" rad) $ histo1D (binD (-pi) 30 pi)

sd12Histo :: YodaHisto1D
sd12Histo = YodaHisto "/sd12" "$\\sqrt{d_{12}}$ [GeV]" (dsigdXpbY "\\sqrt{d_{12}}" gev) $ histo1D (binD 0 30 300)

tau21Histo :: YodaHisto1D
tau21Histo = YodaHisto "/tau21" "$\\tau_{21}$" (dsigdXpbY "\\tau_{21}" "1") $ histo1D (binD 0 30 30)

tau32Histo :: YodaHisto1D
tau32Histo = YodaHisto "/tau32" "$\\tau_{32}$" (dsigdXpbY "\\tau_{32}" "1") $ histo1D (binD 0 30 30)

dRHisto :: YodaHisto1D
dRHisto = YodaHisto "/deltaR" "$\\Delta R$" (dsigdXpbY "\\Delta R" "rad") $ histo1D (binD 0 25 5)

nObjHisto :: YodaHisto1D
nObjHisto = YodaHisto "/n" "multiplicity" (dsigdXpbY "n" "\\mathrm{unit}") $ histo1D (binD 0 5 5)


filling :: (Monad m, Num v, V.Unbox v, Bin b)
        => Histogram b v -> Consumer (v, BinValue b) m (Histogram b v)
filling = foldlC (flip $ fill (+))


-- shouldn't I be able to write this more cleanly?
fillingYH :: (Monad m, Num v, V.Unbox v, Bin b)
          => YodaHisto b v -> Consumer (v, BinValue b) m (YodaHisto b v)
fillingYH yh = flip (set yhHisto) yh <$> filling (view yhHisto yh)


fillAll :: (Monad m, Applicative f, Foldable f)
         => Consumer (v, a) m b -> Consumer (v, f a) m b
fillAll c = CL.map sequenceA =$= CL.concat =$= c


fillFirst :: (Monad m, Applicative f, Foldable f)
          => Consumer (v, a) m b -> Consumer (v, f a) m b
fillFirst c = CL.map (traverse firstF) =$= CL.concat =$= c


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


firstF :: Foldable f => f a -> Maybe a
firstF = listToMaybe . toList


-- common histograms for LorentzVectors
lvHistos :: (Monad m, HasLorentzVector a) => Consumer (Weighted a) m [YodaHisto1D]
lvHistos = sequenceConduits [ fillingYH ptHisto  <=$= CL.map (fmap lvPt)
                            , fillingYH etaHisto <=$= CL.map (fmap lvEta)
                            ] <=$= CL.map (fmap toPtEtaPhiE)

jetTrkHistos :: Monad m => Consumer (Weighted Jet) m [YodaHisto1D]
jetTrkHistos = sequenceConduits [ fillingYH sumTrkPtHisto    <=$= CL.map (fmap sumTrkPt)
                                , fillingYH sumSV1TrkPtHisto <=$= CL.map (fmap sumSV1TrkPt)
                                , fillingYH bFragHisto       <=$= CL.map (fmap bFrag)
                                ] <=$= CL.filter ((> 0) . length . jSV1Tracks . snd)

jetsHistos :: Monad m => Consumer (Weighted [Jet]) m [YodaHisto1D]
jetsHistos = fmap ((path %~ ("/jets" <>)) . (xLabel %~ ("small-$R$ jet " <>)))
             <$> (fillAll jetTrkHistos =++= fillAll lvHistos)

jet0Histos :: Monad m => Consumer (Weighted [Jet]) m [YodaHisto1D]
jet0Histos = fmap ((path %~ ("/jet0" <>)) . (xLabel %~ ("leading small-$R$ jet " <>)))
             <$> (fillFirst jetTrkHistos =++= fillFirst lvHistos)

jetHistos :: Monad m => Consumer (Weighted Event) m [YodaHisto1D]
jetHistos = (jetsHistos =++= jet0Histos) <=$= CL.map (fmap _jets)

{-
ljetHs :: Monad m => Consumer (Weighted LargeJet) m [YodaHisto1D]
ljetHs = (fillingYH mHisto <=$= CL.map (fmap ljM))
         =:= (fillingYH sd12Histo <=$= CL.map (fmap ljSD12))
         =:= lvHistos


ljetsHistos :: Monad m => Consumer (Weighted LargeJets) m [YodaHisto1D]
ljetsHistos = fmap ((path %~ ("/ljets" <>)) . (xLabel %~ ("large-$R$ jet " <>)))
              <$> fillAll ljetHs


ljet0Histos :: Monad m => Consumer (Weighted LargeJets) m [YodaHisto1D]
ljet0Histos = fmap ((path %~ ("/ljet0" <>)) . (xLabel %~ ("leading large-$R$ jet " <>)))
              <$> fillFirst ljetHs


ljetHistos :: Monad m => Consumer (Weighted Event) m [YodaHisto1D]
ljetHistos = (ljetsHistos =++= ljet0Histos) <=$= CL.map (fmap eLargeJets)


eljetHisto :: Monad m => Consumer (Weighted Event) m YodaHisto1D
eljetHisto = ((path %~ ("/elljet0" <>)) . (xLabel %~ ("electron-large-$R$ jet " <>)))
             <$> fillFirst (fillingYH dRHisto) <=$= CL.map (fmap f)
    where f evt = flip minDR (eElectrons evt) =<< leading (eLargeJets evt)

tjetHistos :: Monad m => Consumer (Weighted Event) m [YodaHisto1D]
tjetHistos = fmap ((path %~ ("/tjets" <>)) . (xLabel %~ ("track jet " <>)))
             <$> fillAll lvHistos <=$= CL.map (fmap eTrackJets)
-}

electronsHistos :: Monad m => Consumer (Weighted Event) m [YodaHisto1D]
electronsHistos = fmap ((path %~ ("/electrons" <>)) . (xLabel %~ ("electron " <>)))
                  <$> fillAll lvHistos <=$= CL.map (fmap _electrons)


muonsHistos :: Monad m => Consumer (Weighted Event) m [YodaHisto1D]
muonsHistos = fmap ((path %~ ("/muons" <>)) . (xLabel %~ ("muon " <>)))
              <$> fillAll lvHistos <=$= CL.map (fmap _muons)

metHisto :: Monad m => Consumer (Weighted Event) m YodaHisto1D
metHisto = ((path %~ ("/met" <>)) . (xLabel %~ ("$E_{\\mathrm{T}}^{\\mathrm{miss}}$ " <>)))
             <$> fillingYH ptHisto <=$= CL.map (fmap (lvPt . _met))

eventHistos :: Monad m => Consumer (Weighted Event) m [YodaHisto1D]
eventHistos = {- eljetHisto =:= -} metHisto =:= jetHistos
                         =++= electronsHistos =++= muonsHistos
                         -- =++= ljetHistos =++= tjetHistos


channel :: Monad m => Text -> (Event -> Bool) -> Consumer (Weighted Event) m [YodaHisto1D]
channel n f = (fmap.fmap) (path %~ (n <>)) $ filterC (f . snd) =$= eventHistos


channelHistos :: Monad m => Consumer (Weighted Event) m (Int, ZipList YodaHisto1D)
channelHistos = getZipConduit $ (,) <$> ZipConduit lengthC <*> ZipConduit (ZipList . concat <$> sequenceConduits [ CL.map (fmap pruneJets) =$= channel "/elmujj/inclusive" elmujj ])
