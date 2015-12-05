module Main where

import qualified Data.ByteString.Lazy as BSL

import Data.HEP.Atlas.Stream
import Data.HEP.Atlas.TopTree

main :: IO ()
main = BSL.putStr . encodeList . parseTree =<< BSL.getContents
