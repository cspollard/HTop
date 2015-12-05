-- from
-- https://stackoverflow.com/questions/6205294/binary-serialization-for-lists-of-undefined-length-in-haskell

module Data.HEP.Atlas.Stream where

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
                    _ -> fail "unexpected Word8 in decodeElem"


-- TODO
-- this is probably suboptimal: converting between lazy and strict
-- bytestrings over and over
-- look for a solution using pushCheck instead of pushChecks...
decodeList :: Binary a => BSL.ByteString -> [a]
decodeList bs = case runGetIncremental decodeElem `pushChunks` bs of
                    Fail _ _ err -> error err
                    Done _ _ Nothing -> []
                    Done bs' _ (Just x) -> x : decodeList (BSL.fromStrict bs')
                    _ -> error "incomplete inputs."
