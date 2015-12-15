module Main where

import qualified Data.ByteString.Lazy as BSL

import Data.HEP.Atlas.Stream
import Data.HEP.Atlas.Event

main :: IO ()
main = do
        evts <- return . decodeList =<< BSL.getContents :: IO Events
        print evts
