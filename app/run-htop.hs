{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Main where

import           Codec.Compression.GZip   (compress)
import qualified Control.Foldl            as F
import           Control.Lens
import           Control.Monad            (forM, when)
import qualified Data.ByteString.Lazy     as BS
import qualified Data.Map.Strict          as M
import           Data.Semigroup
import           Data.Serialize           (encodeLazy)
import qualified Data.Text                as T
import           GHC.Float
import           List.Transformer
import qualified List.Transformer         as L
import           Options.Generic
import           System.IO                (hFlush, stdout)

import           Data.Atlas.Event
import           Data.Atlas.Histogramming
import           Data.Atlas.Selection
import           Data.TFile
import           Data.TTree

type TreeName = String
type SystName = T.Text

data Args =
  Args
    { outfile :: String
    , infiles :: String
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
            (fillFile [("nominal", M.singleton "nominal" <$> readBranch "EvtW")])
            (return Nothing)
            return

  imh <- F.impurely L.foldM f fnl

  putStrLn ("writing to file " ++ outfile args) >> hFlush stdout

  BS.writeFile (outfile args) (compress . encodeLazy $ imh)


fillFile
  :: [(TreeName, TR IO (M.Map SystName Double))]
  -> Maybe (Int, Double, SystMap YodaFolder)
  -> String
  -> IO (Maybe (Int, Double, SystMap YodaFolder))
fillFile systs m fn = do
  putStrLn ("analyzing file " ++ fn) >> hFlush stdout

  -- check whether or not this is a data file
  f <- tfileOpen fn
  tw <- ttree f "sumWeights"
  (L.Cons (dsidc :: CInt) _) <- next $ runTTreeL (readBranch "dsid") tw

  let dsid = fromEnum dsidc
      fo = F.Fold (+) (0 :: Float) id

  sow' <-
    F.purely L.fold fo
      $ runTTreeL (readBranch "totalEventsWeighted") tw

  let sow = float2Double sow'

  systHs <- fmap M.unions . forM systs $ \(tn, readws) -> do
    t <- ttree f tn
    putStrLn $ "looping over tree " <> tn

    -- deal with possible missing trees
    nt <- isNullTree t
    when nt $ do
      putStrLn $ "missing tree " <> tn <> " in file " <> fn <> "."
      putStrLn "continuing."

    let l = if nt then L.empty else runTTreeL tmp t
        tmp = do
          evt <- overlapRemoval . pruneJets <$> readEvent (dsid == 0)
          if dsid == 0
            then return (evt, M.singleton "data" 1)
            else do
              ws <- readws
              return (evt, ws)

    F.purely L.fold defHs l

  tfileClose f

  case m of
      Nothing -> return $ Just (dsid, sow, systHs)
      Just (dsid', n, hs') -> do
        when (dsid /= dsid') $ error "attempting to analyze different dsids in one run!!!"
        return $ Just (dsid, n+sow, M.unionWith mergeYF systHs hs')

  where
    defHs :: F.Fold (Event, SystMap Double) (SystMap YodaFolder)
    defHs = withWeights . channel "/elmujj" elmujj $ eventHs
