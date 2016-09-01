-- a catch-all of helper functions that eventually need to find a new
-- home

module Data.Atlas.Helpers where

import Conduit
import qualified Data.Conduit.List as CL
import Data.Atlas.Event

cut :: Monad m => (a -> Bool) -> Conduit a m a
cut = CL.filter


minDR :: (HasLorentzVector a, HasLorentzVector b, Foldable f, Functor f) => a -> f b -> Maybe Double
minDR v vs = if null vs then Nothing else Just . minimum $ fmap (lvDR v) vs

ljetSelection :: Electrons -> LargeJet -> Bool
ljetSelection els lj = maybe True (> 1.0) $ minDR lj els


nLep :: Event -> Int
nLep = (+) <$> (length . eElectrons) <*> (length . eMuons)

evtSelection :: Monad m => Conduit Event m Event
evtSelection = cut ((== 2) . nLep) =$= cut (not . null . length . eLargeJets)


conduitEncode :: (Monad m, Serialize a) => Conduit a m ByteString
conduitEncode = CL.map encode

conduitDecode :: (Monad m, Serialize a) => Conduit ByteString m a
conduitDecode = do mbs <- await
                   case mbs of
                       Nothing -> return ()
                       -- some streams seem to return an empty
                       -- ByteString instead of Nothing?????
                       Just "" -> conduitDecode
                       Just bs -> go $ runGetPartial get bs

    where
        -- TODO
        -- fail
        go (Fail x z) = fail (x ++ " " ++ show z)
        go (Done x rest) = leftover rest >> yield x >> conduitDecode
        go (Partial f) = go =<< (f . fromMaybe "") <$> await
