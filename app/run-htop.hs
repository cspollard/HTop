{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Lens

import Conduit
import Data.Conduit.Binary (sourceLbs)
import Data.Conduit.Zlib (gzip)
import Data.Serialize (encodeLazy)
import Data.Serialize.ZipList ()

import Control.Monad (forM, when)
import Control.Applicative (liftA2, ZipList(..))
import Data.Traversable (forM)

import Options.Generic


import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M

import Data.TTree
import Data.Atlas.Histograms
import Data.Atlas.Event
import Data.Atlas.Sample
import Data.Atlas.Selection
import Data.YODA.Obj
import Data.Atlas.CrossSections

data Args = Args { outfile :: String
                 , infiles :: String
                 , xsecfile :: String
                 } deriving (Show, Generic)

instance ParseRecord Args where

everyC :: Monad m => Int -> (Int -> a -> m ()) -> Conduit a m a
everyC n f = loop 0
    where loop i = do mx <- await
                      case mx of
                           Just x -> when (i `mod` n == 0) (lift $ f i x) >> yield x >> loop (i+1)
                           Nothing -> return ()

main :: IO ()
          -- read in cmd line args
main = do args <- getRecord "run-hs" :: IO Args

          xsecs <- readXSecFile (xsecfile args)

          -- get the list of input trees
          fs <- lines <$> readFile (infiles args)

          let systs = [nominal, pileupUp, pileupDown]
          samps <- forM fs $
                        \f -> do putStrLn $ "analyzing events in file " ++ f
                                 wt <- ttree "sumWeights" f
                                 tt <- ttree "nominal" f
                                 s <- foldl1 addSampInfo <$> (project wt $$ sinkList)
                                 (n, hs) <- case dsid s of
                                               0 -> fmap obj $
                                                       project tt
                                                       =$= everyC 1000 printIE
                                                       $$ mapC dataEvent
                                                       =$= foldlC feed (withLenF $ channel "/elmujj" (elmujj . snd) dataEventObjs)

                                               _ -> fmap obj $
                                                       runTTree (readEventSysts systs) tt
                                                       =$= everyC 1000 printIE
                                                       $$ foldlC feed (withLenF . channel "/elmujj" (elmujj . (M.! "nominal")) $ mcEventObjs systs)

                                 putStrLn $ show n ++ " events analyzed in file " ++ f ++ ".\n"
                                 return (s, ZipList hs)

          let m = IM.fromListWith (\(s, h) (s', h') -> (addSampInfo s s', liftA2 mergeYO h h')) $ map ((,) <$> fromEnum . dsid . fst <*> id) samps


          let scaledHists = flip IM.mapWithKey m $
                                \ds (s, hs) -> case ds of
                                                    0 -> hs
                                                    _ -> over (traverse . noted . _H1DD) (flip scaledBy $ (xsecs IM.! ds) / totalEventsWeighted s) hs


          runResourceT $ sourceLbs (encodeLazy scaledHists) =$= gzip $$ sinkFile (outfile args)

    where
        printIE i _ = putStrLn (show i ++ " events analyzed")
