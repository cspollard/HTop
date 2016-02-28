-- from
-- https://stackoverflow.com/questions/6205294/binary-serialization-for-lists-of-undefined-length-in-haskell

module Data.HEP.Atlas.Stream where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS


data Stream a = Stream { unStream :: [a] } deriving (Eq, Ord, Show)

instance Binary a => Binary (Stream a) where
    put (Stream []) = putWord8 0
    put (Stream (x:xs)) = putWord8 1 >> put x >> put (Stream xs)

    -- TODO
    -- there is a memory leak here.
    get = do
            x <- decodeElem
            case x of
                Nothing -> return (Stream [])
                Just y -> do
                    xs <- get
                    return (Stream (y : unStream xs))


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
