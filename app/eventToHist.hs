{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS

import Data.Conduit
import qualified Data.Conduit.List as CL

import qualified Data.Conduit.Binary as B

import Data.HEP.Atlas.Tree
import Data.HEP.Atlas.Sample
import Data.HEP.Atlas.Event
import Data.HEP.Atlas.TopTree
import Data.HEP.Atlas.Histograms

import System.IO (stdout, stdin)

import Data.Maybe (fromJust)

import Data.Histogram
import Data.Builder

import Debug.Trace

main :: IO ()
main = do
        (s, samp) <- B.sourceHandle stdin
                        $$+ (conduitDecode :: Conduit BS.ByteString IO SampleInfo)
                        =$= (fromJust <$> await)

        hists <- s $$+- conduitDecode =$= CL.map traceShowId
                   =$= CL.fold build (eventSystHists ["nominal"])

        CL.sourceList (concatMap concat $ built hists)
            $$ conduitEncode =$= B.sinkHandle stdout

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
