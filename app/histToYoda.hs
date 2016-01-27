{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import Control.Monad (forM_)

import Data.HEP.Atlas.Histograms 
import Data.HEP.Atlas.Stream

main :: IO ()
main = do
        hists <- decodeList <$> BSL.getContents

        forM_ hists $
            putStr . T.unpack . showHist "/HTop/"
