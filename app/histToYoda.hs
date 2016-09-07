{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Lens

import Conduit
import Data.Conduit.Zlib (ungzip)
import Data.Serialize (decode)
import Data.Serialize.ZipList ()

import Control.Monad (forM_)
import Control.Applicative (ZipList(..))

import Data.ByteString.Lazy (fromChunks, toStrict)

import Options.Generic

import qualified Data.IntMap.Strict as IM

import Data.YODA.Obj

data Args = Args { outfolder :: String
                 , infile :: String
                 , lumi :: Double
                 } deriving (Show, Generic)

instance ParseRecord Args where

main :: IO ()
          -- read in cmd line args
main = do args <- getRecord "histToYoda" :: IO Args
          eim <- decode . toStrict . fromChunks <$> runResourceT (sourceFile (infile args) $$ ungzip =$= sinkList) :: IO (Either String (IM.IntMap (ZipList YodaObj)))
          print eim

          case eim of
               Left err -> print err
               Right im -> forM_ (IM.toList im) $
                            \(ds, hs) -> runResourceT $ yieldMany (getZipList hs)
                                                        =$= mapC (if ds == 0 then id else over (noted . _H1DD) (`scaledBy` lumi args))
                                                        =$= mapC printYObj
                                                        $$ sinkFile (outfolder args ++ '/' : show ds ++ ".yoda")
