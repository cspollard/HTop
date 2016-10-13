{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Lens
import qualified Control.Foldl as F
import List.Transformer

import Conduit
import Data.Conduit.Binary (sourceLbs)
import Data.Conduit.Zlib (gzip)
import Data.Serialize (encodeLazy)
import Data.Serialize.ZipList ()

import Control.Monad (forM, when)
import Control.Applicative (liftA2, ZipList(..))

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

everyL :: Monad m => Int -> (Int -> a -> m ()) -> ListT m a -> ListT m a
everyL n f = loop 0
    where
        loop i l = ListT $ do
            mx <- next l
            case mx of
                Cons x l' -> when (i `mod` n == 0) (f i x) >> (return . Cons x . loop (i+1)) l'
                Nil -> return Nil

main :: IO ()
          -- read in cmd line args
main = do args <- getRecord "run-hs" :: IO Args

          xsecs <- readXSecFile (xsecfile args)

          -- get the list of input trees
          fs <- lines <$> readFile (infiles args)

          let systs = [nominal, pileupUp, pileupDown]
          samps <- forM fs $
            \f -> do
                putStrLn $ "analyzing events in file " ++ f
                wt <- ttree "sumWeights" f
                tt <- ttree "nominal" f
                s <- fold addSampInfo defsi id $ project wt
                (n, hs) <-
                    case dsid s of
                       0 -> let f' = withLenF (channel "/elmujj" (elmujj . snd) dataEventObjs)
                                        <$= dataEvent
                            in  F.purely fold f' $ everyL 1000 printIE $ project tt

                       _ -> let f' = withLenF . channel "/elmujj" (elmujj . (M.! "nominal")) $ mcEventObjs systs
                            in  F.purely fold f' $ everyL 1000 printIE $ runTTree (readEventSysts systs) tt

                putStrLn $ show (n :: Int) ++ " events analyzed in file " ++ f ++ ".\n"
                return (s, ZipList hs)

          let m = IM.fromListWith (\(s, h) (s', h') -> (addSampInfo s s', liftA2 mergeYO h h')) $ map ((,) <$> fromEnum . dsid . fst <*> id) samps


          let scaledHists = flip IM.mapWithKey m $
                                \ds (s, hs) -> case ds of
                                                    0 -> hs
                                                    _ -> over (traverse . noted . _H1DD) (flip scaledBy $ (xsecs IM.! ds) / totalEventsWeighted s) hs


          runResourceT $ sourceLbs (encodeLazy scaledHists) =$= gzip $$ sinkFile (outfile args)

    where
        printIE :: Int -> a -> IO ()
        printIE i _ = putStrLn (show i ++ " events analyzed")
        defsi = SampleInfo (-1) 0 0
