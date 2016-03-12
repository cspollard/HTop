{-# LANGUAGE OverloadedStrings #-}

module Main where

-- TODO
-- parallelism
-- import Control.Parallel.Strategies (using, rseq, parBuffer)

import qualified Data.ByteString.Lazy as BSL
import Data.Attoparsec.ByteString.Lazy

-- import Data.Binary (encode)
-- import Data.HEP.Atlas.TopTree
-- import Data.HEP.Atlas.CrossSections
import Data.Maybe (fromJust)
import System.Environment (getArgs)

import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.HEP.Atlas.Tree

-- TODO
-- get conduit in here
-- sourceList . toChunks <$> BSL.readFile csname
-- or something

main :: IO ()
main = do
    csname <- head <$> getArgs
    fmap BSL.toChunks (BSL.readFile csname) >>= (\x -> CL.sourceList x =$= file $$ CL.mapM_ print)
