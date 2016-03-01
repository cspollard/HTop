{-# LANGUAGE DeriveGeneric #-}

module Data.HEP.Atlas.Sample where

import Data.Monoid ((<>), mconcat)

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import Data.HEP.Atlas.Event
import Data.HEP.Atlas.Stream
import qualified Data.ByteString.Lazy as BSL

data Sample = SingleSample Int Int Double Double Events
            | CombinedSample [Sample]

encodeSample :: Sample -> BSL.ByteString
encodeSample (SingleSample w x y z evts) =
        runPut (putWord8 0 >> put w >> put x >> put y >> put z) <> encodeList evts
encodeSample (CombinedSample ss) = runPut (putWord8 1) <> mconcat (map encodeSample ss)

-- TODO
-- HERE HERE HERE
decodeSample :: BSL.ByteString -> Sample
decodeSample bs = case runGetOrFail (getWord8) bs of
                    0 -> case runGetOrFail (SingleSample <$> get <*> get <*> get <*> get) bs of
                            Right (bs', _, f) -> f $ decodeList bs'
                            Left _ -> error "can't decode sample."


        getSample :: Get (BSL.ByteString -> Sample)
        getSample = do
                        x <- getWord8
                        case x of
                            0 -> fmap (. decodeList) $ SingleSample <$> get <*> get <*> get <*> get
                            1 -> CombinedSample . decodeList

dsids :: Sample -> [Int]
dsids (SingleSample x _ _ _ _) = [x]
dsids (CombinedSample ss) = concatMap dsids ss

numEvents :: Sample -> Int
numEvents (SingleSample _ x _ _ _) = x
numEvents (CombinedSample ss) = sum . map numEvents $ ss

sumWeights :: Sample -> Double
sumWeights (SingleSample _ _ x _ _) = x
sumWeights (CombinedSample ss) = sum . map sumWeights $ ss

crossSection :: Sample -> Double
crossSection (SingleSample _ _ _ x _) = x
crossSection (CombinedSample ss) = sum . map crossSection $ ss

events :: Sample -> Events
events (SingleSample _ _ _ _ x) = x
events (CombinedSample ss) = concatMap events ss
