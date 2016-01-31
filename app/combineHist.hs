{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as BSC
import Data.HEP.Atlas.CrossSections
import Data.Attoparsec.ByteString.Char8 (parseOnly)

main :: IO ()
main = do
    bs <- BSC.getContents

    print $ parseOnly crossSectionInfo bs
