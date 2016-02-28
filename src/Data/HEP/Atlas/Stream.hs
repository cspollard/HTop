-- from
-- https://stackoverflow.com/questions/6205294/binary-serialization-for-lists-of-undefined-length-in-haskell

module Data.HEP.Atlas.Stream where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Data.List (unfoldr)



encodeList :: Binary a => [a] -> BSL.ByteString
encodeList = runPut . encodeList'
    where
        encodeList' :: Binary a => [a] -> Put
        encodeList' [] = putWord8 0
        encodeList' (x:xs) = putWord8 1 >> put x >> encodeList' xs


decodeElem :: Binary a => Get (Maybe a)
decodeElem =  do
                t <- getWord8
                case t of
                    0 -> return Nothing
                    1 -> fmap Just get
                    _ -> fail "unexpected Word8 in decodeElem"


-- TODO
-- is this efficient?
decodeList :: Binary a => BSL.ByteString -> [a]
decodeList bs = decodeList' (runGetIncremental decodeElem) (BSL.toChunks bs)
    where
        decodeList' :: Binary a => Decoder (Maybe a) -> [BS.ByteString] -> [a]
        decodeList' _ [] = []
        decodeList' d (bs':bss) = case d `pushChunk` bs' of
                    Fail _ _ err -> error err
                    Done _ _ Nothing -> []
                    Done bs'' _ (Just x) -> x : decodeList' (runGetIncremental decodeElem) (bs'':bss)
                    -- Partial
                    p -> decodeList' p bss
