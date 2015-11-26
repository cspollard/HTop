-- from
-- https://stackoverflow.com/questions/6205294/binary-serialization-for-lists-of-undefined-length-in-haskell

module Data.Atlas.Stream where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Data.List (unfoldr)
import Control.Monad (liftM2)


encodeList' :: Binary a => [a] -> Put
encodeList' [] = putWord8 0
encodeList' (x:xs) = putWord8 1 >> put x >> encodeList' xs

encodeList :: Binary a => [a] -> BSL.ByteString
encodeList = runPut . encodeList'

decodeElem :: Binary a => Get (Maybe a)
decodeElem =  do
                t <- getWord8
                case t of
                    0 -> return Nothing
                    1 -> fmap Just get


decodeList :: Binary a => BSL.ByteString -> [a]
decodeList bs = case runGetIncremental decodeElem `pushChunks` bs of
                    Fail _ _ err -> error err
                    Done _ _ Nothing -> []
                    Done bs' _ (Just x) -> x : decodeList (BSL.fromStrict bs')
                    _ -> error "incomplete inputs."


data Stream a = Stream { unstream :: [a] }


instance Binary a => Binary (Stream a) where

    put (Stream []) = putWord8 0
    put (Stream (x:xs)) = putWord8 1 >> put x >> put (Stream xs)

    get = do
        t <- getWord8
        case t of
            0 -> return (Stream [])
            1 -> do
                   x <- get
                   Stream xs <- get
                   return $ Stream (x:xs)


runGets :: Get a -> BSL.ByteString -> Stream a
runGets g = Stream . unfoldr (decode' d) . BSL.toChunks
    where d = runGetIncremental g

decode' :: Decoder a -> [BS.ByteString] -> Maybe (a, [BS.ByteString])
decode' _ [] = Nothing
decode' d (bs:bss) = case d `pushChunk` bs of
                     Fail _ _ str  -> error str
                     Done x' _ a   -> Just (a, x':bss)
                     k@(Partial _) -> decode' k bss

