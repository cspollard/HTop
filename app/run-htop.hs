{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}

module Main where

import qualified Control.Foldl    as F
import           Control.Lens
import           Control.Monad    (unless, when)
import qualified Data.Map.Strict  as M
import           Data.Semigroup
import qualified Data.Text        as T
import           GHC.Float
import           Options.Generic
import qualified Pipes            as P
import qualified Pipes.Prelude    as P
import           System.IO        (BufferMode (..), hSetBuffering, stdout)

import           Atlas
import           BFrag.Event
import           Data.TFile
import           Data.TTree


-- TODO

-- we should also use the comonad instance of folds to remove the ~doubling of
-- memory usage on >1 file.

data Args =
  Args
    { outfile :: String
    , infiles :: String
    } deriving (Show, Generic)

instance ParseRecord Args where

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering

  -- read in cmd line args
  args <- getRecord "run-hs" :: IO Args

  -- get the list of input trees
  fns <- filter (not . null) . lines <$> readFile (infiles args)

  imh <-
    P.foldM
      (fillFile ("nominal", treeSysts))
      (return Nothing)
      return
      (P.each fns)

  maybe
    (error "no input files!")
    ( \x -> do
      putStrLn ("writing to file " ++ outfile args)
      encodeFile (outfile args) x
    )
    imh


fillFile
  :: (String, [String])
  -> Maybe (Int, Sum Double, Folder (Vars YodaObj))
  -> String
  -> IO (Maybe (Int, Sum Double, Folder (Vars YodaObj)))
fillFile (nom, systs') m fn = do
  putStrLn $ "analyzing file " <> fn

  -- check whether or not this is a data file
  f <- tfileOpen fn
  tw <- ttree f "sumWeights"
  (Just (dsidc :: CInt)) <-
    runTR tw . P.head $ produceTTree (readBranch "dsid")

  let dsid = fromEnum dsidc
      fo = F.Fold (+) (0 :: Float) id
      systs = if dsid == 410000 then systs' else []

  sow <-
    runTR tw
    . fmap (Sum . float2Double)
    . F.purely P.fold fo
    $ produceTTree (readBranch "totalEventsWeighted")

  print sow

  let loopTree tn = do
        t <- ttree f tn
        putStrLn $ "looping over tree " <> tn

        -- deal with possible missing trees
        nt <- isNullTree t
        when nt $ do
          putStrLn $ "missing tree " <> tn <> " in file " <> fn <> "."
          putStrLn "continuing."

        let l = unless nt . produceTTree $ readEvent dmc
            dmc =
              if dsid == 0
                then Data'
                else MC' $
                  if dsid == 410000 && tn == "nominal"
                    then AllVars
                    else NoVars

        runTR t . fmap (tn,) $! F.purely P.fold eventHs l

  systHs :: Folder (M.Map T.Text YodaObj) <-
    Folder
    . transposeM
    . M.fromList
    . fmap (over _1 T.pack . fmap (folderToMap . fmap (view nominal)))
    <$> mapM loopTree systs

  nomHs <- snd <$> loopTree nom
  let hs =
        if dsid == 0 || null systHs
          then nomHs
          else
            inF2
              (M.intersectionWith (\n v -> over variations (<> v) n))
              nomHs
              systHs

  putStrLn $ "closing file " <> fn
  tfileClose f

  case m of
      Nothing ->
        hs `seq` return (Just (dsid, sow, hs))
      Just (dsid', n, hs') -> do
        when (dsid /= dsid')
          $ error "attempting to analyze different dsids in one run!!!"
        let n' = mappend n sow
            sm' = mappend hs hs'
        sm' `seq` return (Just (dsid, n', sm'))


transposeM
  :: forall k k' a. (Ord k, Ord k')
  => M.Map k (M.Map k' a) -> M.Map k' (M.Map k a)
transposeM = M.foldrWithKey f M.empty
  where
    f :: k -> M.Map k' a -> M.Map k' (M.Map k a) -> M.Map k' (M.Map k a)
    f k inmap outmap = M.unionWith (<>) outmap $ M.singleton k <$> inmap
