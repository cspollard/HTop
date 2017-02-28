{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Main where

import           Control.Applicative      (ZipList (..), liftA2)
import           Control.Arrow            ((&&&))
import qualified Control.Foldl            as F
import           Control.Lens
import           Control.Monad            (forM, when)
import           Data.Biapplicative
import           Data.List                (isInfixOf)
import           Data.Maybe               (fromMaybe)
import           Data.Semigroup
import           Data.Serialize           (encodeLazy)
import           GHC.Float
import           List.Transformer
import           Options.Generic

import qualified Data.IntMap.Strict       as IM
import qualified Data.Map.Strict          as M
import qualified Data.Text                as T
import qualified List.Transformer         as L
import           System.IO                (hFlush, stdout)

import           Data.Atlas.CrossSections
import           Data.Atlas.Event
import           Data.Atlas.Histogramming
import           Data.Atlas.Selection
import           Data.TFile
import           Data.TTree

type TreeName = String
type SystName = T.Text

data Args =
  Args
    { outfile  :: String
    , infiles  :: String
    , xsecfile :: String
    } deriving (Show, Generic)

instance ParseRecord Args where

main :: IO ()
main = do
  -- read in cmd line args
  args <- getRecord "run-hs" :: IO Args

  -- get the list of input trees
  fns <- filter (not . null) . lines <$> readFile (infiles args)

  let fnl = L.select fns :: L.ListT IO String
      f = F.FoldM
            (fillFile [("nominal", M.singleton "nominal" <$> readBranch "eventWeight")])
            (return Nothing)
            return

  imh <- F.impurely L.foldM f fnl

  return ()


fillFile
  :: [(TreeName, TR IO (M.Map SystName Double))]
  -> Maybe (Int, Double, SystMap YodaFolder)
  -> String
  -> IO (Maybe (Int, Double, SystMap YodaFolder))
fillFile systs m fn = do
  putStrLn ("analyzing file " ++ fn) >> hFlush stdout

  -- check whether or not this is a data file
  let (dsid :: Int) =
        if "data15_13TeV" `isInfixOf` fn || "data16_13TeV" `isInfixOf` fn
          then 0
          -- this works on files of the form
          -- /blah/blah/blah/blah.blah.blah.DSID.blah/blah
          else
            fn
              & read . T.unpack . (!! 3)
                . T.split (== '.') . (!! 1)
                . reverse . T.split (== '/') . T.pack

  f <- tfileOpen fn
  tw <- ttree f "sumOfWeights"
  let fo = F.Fold (biliftA2 (+) (+)) (0 :: CInt, 0 :: Float) id
  (nevents', sow') <-
    F.purely L.fold fo
      $ runTTreeL
          ((,) <$> readBranch "nevents" <*> readBranch "sumOfWeights")
          tw

  let (nevents :: Int) = fromEnum nevents'
      sow = float2Double sow'

  systHs <- fmap M.unions . forM systs $ \(tn, readws) -> do
    t <- ttree f tn
    putStrLn $ "looping over tree " <> tn

    -- deal with possible missing trees
    nt <- isNullTree t
    when nt $ do
      putStrLn $ "missing tree " <> tn <> " in file " <> fn <> "."
      putStrLn "continuing."

    let l =
          if nt
            then L.empty
            else runTTreeL tmp t
        tmp = do
          evt <- overlapRemoval <$> fromTTree
          liftIO $ print evt
          if dsid == 0
            then return (evt, M.singleton "data" 1)
            else do
              ws <- readws
              liftIO $ print ws
              return (evt, ws)

    F.purely L.fold defHs l

  tfileClose f

  case m of
      Nothing -> return $ Just (dsid, nevents, systHs)
      Just (dsid', n, hs') -> do
        when (dsid /= dsid') $ error "attempting to analyze different dsids in one run!!!"
        return $ Just (dsid, n+nevents, M.unionWith mergeYF systHs hs')

  where
    defHs :: F.Fold (Event, SystMap Double) (SystMap YodaFolder)
    defHs = withWeights eventHs
