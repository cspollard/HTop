{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import qualified Data.ByteString.Char8 as BSC
import Data.HEP.Atlas.CrossSections
import Data.Attoparsec.ByteString.Char8 (parseOnly)

main :: IO ()
main = do
    fin <- head <$> getArgs
    xsecs <- parseOnly crossSectionInfo <$> BSC.readFile fin

    print "THIS CURRENTLY DOES NOTHING."
