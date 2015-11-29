module Main where

import qualified Data.ByteString.Lazy as BSL

import Data.Atlas.Stream
import Data.Atlas.Event

main :: IO ()
main = do
        evts <- return . decodeList =<< BSL.getContents :: IO Events
        print evts
