{-# LANGUAGE OverloadedStrings, FlexibleContexts, DeriveGeneric, TupleSections, TypeOperators #-}

module Data.HEP.Atlas.Histograms where

import Control.Arrow
import Data.Monoid

import qualified Data.Vector as V

import Data.Text (Text)
import qualified Data.Text as T

import Data.Maybe (listToMaybe, fromJust)

import Data.Foldable (Foldable(..), toList)

import Data.Histogram

import Data.Serialize (Serialize(..))
import GHC.Generics (Generic)

import Data.HEP.Atlas.Event
import Data.HEP.LorentzVector

import qualified Data.Map as M


ptHisto :: Histo1D
ptHisto = histogram (bin1D 50 (0, 500)) mempty

eHisto :: Histo1D
eHisto = histogram (bin1D 50 (0, 500)) mempty

mHisto :: Histo1D
mHisto = histogram (bin1D 50 (0, 200)) mempty

etaHisto :: Histo1D
etaHisto = histogram (bin1D 50 (-3, 3)) mempty

phiHisto :: Histo1D
phiHisto = histogram (bin1D 50 (-pi, pi)) mempty


-- a YodaHisto is just a histogram with some annotations.
data YodaHisto b val = YodaHisto {
                    yhAnnots :: M.Map Text Text, 
                    yhHisto :: !(Histogram b val)
                    } deriving (Generic, Show)

instance (Serialize val, Serialize b) => Serialize (YodaHisto b val) where

instance Functor (YodaHisto b) where
    fmap f (YodaHisto ann h) = YodaHisto ann $ fmap f h

alterAnnots :: (M.Map Text Text -> M.Map Text Text) -> YodaHisto b val -> YodaHisto b val
alterAnnots f (YodaHisto yha yhh) = YodaHisto (f yha) yhh

alterHisto :: (Histogram b val -> Histogram b val') -> YodaHisto b val -> YodaHisto b val'
alterHisto f (YodaHisto yha yhh) = YodaHisto yha $ f yhh

type YodaHisto1D = YodaHisto (Bin1D Double) (Dist1D Double)


-- TODO
-- generalize
haddUnsafe :: YodaHisto1D -> YodaHisto1D -> YodaHisto1D
haddUnsafe (YodaHisto _ h) (YodaHisto an' h') = YodaHisto an' (fromJust $ h `hadd` h')


yodaHistoBuilder :: [(Text, Text)] -> Histo1D -> Builder (Double, Double) YodaHisto1D
yodaHistoBuilder annots hist = fmap (YodaHisto (M.fromList annots)) $ distBuilder hist <<- second (Z :.)


fillFirst :: Foldable f => Builder (a, b) c -> Builder (a, f b) c
fillFirst b = foldBuilder b <<- \(w, xs) ->
                                    case listToMaybe . toList $ xs of
                                        Just x -> Just (w, x)
                                        Nothing -> Nothing


fillAll :: (Foldable f, Functor f) => Builder (a, b) c -> Builder (a, f b) c
fillAll b = foldBuilder b <<- \(w, xs) -> fmap (w,) xs


-- suite of histograms for LorentzVectors
-- TODO
-- instance Num b => Num (a -> b) where
lvHistos :: HasLorentzVector a => Text -> Text -> Builder (Double, a) [YodaHisto1D]
lvHistos path xtitle = sequenceA [
                      yodaHistoBuilder [("Path", path <> "pt"), ("XLabel", xtitle <> "$p_{\\mathrm T}$ [GeV]")] ptHisto <<- second ((/ 1e3) . lvPt)
                    , yodaHistoBuilder [("Path", path <> "E"), ("XLabel", xtitle <> "$E$ [GeV]")] eHisto <<- second ((/ 1e3) . lvE)
                    , yodaHistoBuilder [("Path", path <> "mass"), ("XLabel", xtitle <> "mass [GeV]")] mHisto <<- second ((/ 1e3) . lvM)
                    , yodaHistoBuilder [("Path", path <> "eta"), ("XLabel", xtitle <> "$\\eta$")] etaHisto <<- second lvEta
                    , yodaHistoBuilder [("Path", path <> "phi"), ("XLabel", xtitle <> "$\\phi$")] phiHisto <<- second lvPhi
                    ] <<- second toPtEtaPhiE



-- TODO
-- we call, e.g. eJets twice per event.
-- could be cleaned up.
eventHistos :: Text -> Builder Event [YodaHisto1D]
eventHistos syst = fmap concat $ sequenceA [
                  fillAll (lvHistos (syst <> "/jets/") "small-$R$ jet ") <<- second eJets
                , fillAll (lvHistos (syst <> "/largejets/") "large-$R$ jet ") <<- second eLargeJets
                , fillAll (lvHistos (syst <> "/trackjets/") "track jet ") <<- second eTrackJets
                , fillAll (lvHistos (syst <> "/electrons/") "electron ") <<- second eElectrons
                , fillAll (lvHistos (syst <> "/muons/") "muon ") <<- second eMuons
                , fillFirst (lvHistos (syst <> "/jet0/") "leading small-$R$ jet ") <<- second eJets
                , fillFirst (lvHistos (syst <> "/largejet0/") "leading large-$R$ jet ") <<- second eLargeJets
                , fillAll (lvHistos (syst <> "/trackjet0/") "leading track jet ") <<- second eTrackJets
                , fillFirst (lvHistos (syst <> "/electron0/") "leading electron ") <<- second eElectrons
                , fillFirst (lvHistos (syst <> "/muon0/") "leading muon ") <<- second eMuons
                , lvHistos (syst <> "/met/") "$E_{\\mathrm T}^{\\mathrm miss}$ " <<- second eMET
                ] <<- (weight syst &&& id)

eventSystHistos :: [Text] -> Builder Event [YodaHisto1D]
eventSystHistos = fmap concat . traverse eventHistos


channel :: Text -> (Event -> Bool) -> [Text] -> Builder Event (Text, [YodaHisto1D])
channel n f systs = fmap (n,) $ foldBuilder (eventSystHistos systs) <<- \e -> if f e then Just e else Nothing


-- TODO
-- event categorization could be cleaner.
channelSystHistos :: [Text] -> Builder Event [(Text, [YodaHisto1D])]
channelSystHistos systs = sequenceA [ channel "elelJ/" (\e -> V.length (eElectrons e) == 2 && V.length (eMuons e) == 0) systs
                                   , channel "elmuJ/" (\e -> V.length (eElectrons e) == 1 && V.length (eMuons e) == 1) systs
                                   , channel "elnuJ/" (\e -> V.length (eElectrons e) == 1 && V.length (eMuons e) == 0) systs
                                   , channel "mumuJ/" (\e -> V.length (eElectrons e) == 0 && V.length (eMuons e) == 2) systs
                                   , channel "munuJ/" (\e -> V.length (eElectrons e) == 0 && V.length (eMuons e) == 1) systs
                                   , channel "nunuJ/" (\e -> V.length (eElectrons e) == 0 && V.length (eMuons e) == 0) systs
                                   ]


showHisto :: Text -> YodaHisto1D -> Text
showHisto path (YodaHisto annots h) = T.unlines $
                            [ "# BEGIN YODA_HISTO1D " <> path', "Path=" <> path', "Type=Histo1D" ] ++
                            -- write annotations
                            map (\(t, a) -> t <> "=" <> a) (M.toList $ M.delete "Path" annots) ++
                            [
                            "Total\tTotal\t" <> distToText (integral h),
                            "Underflow\tUnderflow\t" <> distToText (underflow h),
                            "Overflow\tOverflow\t" <> distToText (overflow h)
                            ] ++
                            map (\((Z :. xmin, Z :. xmax), b) -> T.pack (show xmin ++ "\t" ++ show xmax ++ "\t") <> distToText b) (toTuples h) ++
                            [ "# END YODA_HISTO1D", "" ]

                            where
                                path' = path <> (M.!) annots "Path"
                                distToText (Dist0 sw sw2 ne :. DistWX swx swx2) = T.pack $
                                                show sw ++ "\t" ++
                                                show sw2 ++ "\t" ++
                                                show swx ++ "\t" ++
                                                show swx2 ++ "\t" ++
                                                show ne
