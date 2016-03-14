{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS

import Data.Conduit
import Data.Conduit.Binary
import Data.Conduit.Zlib (gzip, ungzip)
import qualified Data.Conduit.List as CL


import Data.HEP.Atlas.Tree
import Data.HEP.Atlas.Sample
import Data.HEP.Atlas.Event
import Data.HEP.Atlas.TopTree
import Data.HEP.Atlas.Histograms

import System.IO (stdout, stdin)

import Data.Maybe (fromJust)

import Data.Histogram
import Data.Builder

main :: IO ()
main = do
        (s, samp) <- sourceHandle stdin =$= ungzip
                        $$+ (conduitDecode :: Conduit BS.ByteString IO SampleInfo)
                        =$= (fromJust <$> await)


        hists <- s $$+- conduitDecode
                   =$= CL.fold build (eventSystHists ["nominal"])

        let outHists = concatMap concat $ built hists

        ((yield samp =$= conduitEncode) >> (CL.sourceList outHists =$= conduitEncode))
            $$ gzip =$= sinkHandle stdout

{-

-- example cuts
minPt :: HasLorentzVector a => Double -> Cut a
minPt x = (> x) . lvPt . toPtEtaPhiE

maxAbsEta :: HasLorentzVector a => Double -> Cut a
maxAbsEta x = (< x) . abs . lvEta . toPtEtaPhiE

minMV2c20 :: Double -> Cut Jet
minMV2c20 x = (> x) . jMV2c20

nBtags :: Event -> Int
nBtags = nJets $ minPt 25000 `cAnd` maxAbsEta 2.5 `cAnd` minMV2c20 0.7
-}
