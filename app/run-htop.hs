{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}

module Main where

import           Atlas
import           BFrag.Event
import qualified Control.Foldl   as F
import           Control.Monad   (unless, when)
import qualified Data.Map.Strict as M
import           Data.Semigroup
import qualified Data.Text       as T
import           Data.TFile
import           Data.TTree
import           GHC.Float
import           Options.Generic
import           Pipes
import qualified Pipes.Prelude   as P
import           System.IO       (BufferMode (..), hSetBuffering, stdout)


-- TODO
-- we should use the comonad instance of Fold to remove the ~doubling of
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

  mapM_ (fillFile ["nominal"]) fns


-- TODO
-- all these liftIOs
-- could be put into TTree
fillFile
  :: (MonadIO m, MonadFail m)
  => [String]
  -- -> Maybe (Int, Sum Double, Folder (Vars YodaObj))
  -> String
  -- -> m (Maybe (Int, Sum Double, Folder (Vars YodaObj)))
  -> m ()
fillFile systs fn = do
  liftIO . putStrLn $ "analyzing file " <> fn

  -- check whether or not this is a data file
  tfile <- liftIO $ tfileOpen fn
  liftIO . putStrLn $ "checking sumWeights"

  tw <- liftIO $ ttree tfile "sumWeights"
  (Just (dsidc :: CInt)) <-
    P.head $ produceTTree (readBranch "dsid") tw

  let dsid = fromEnum dsidc
      fo = F.Fold (+) (0 :: Float) id

  sow <-
    fmap (Sum . float2Double)
    . F.purely P.fold fo
    $ produceTTree (readBranch "totalEventsWeighted") tw

  liftIO . putStrLn $ "sum of weights: " ++ show (getSum sow)

  let treeProd tr tn = do
        t <- ttree tfile tn

        -- deal with possible missing trees
        nt <- isNullTree t
        when nt $ do
          liftIO . putStrLn $ "missing tree " <> tn <> " in file " <> fn <> "."
          liftIO $ putStrLn "continuing."

        let l = unless nt $ produceTTree tr t

        return (T.pack tn, l)

  truthTree <- snd <$> treeProd readTrue "particleLevel"

  let dmc =
        if dsid == 0
          then Data'
          else MC' $
            -- TODO
            -- we're running all systs here...
            if dsid == 410501 -- && tn == "nominal"
              then AllVars
              else NoVars

  recoTrees <-
    recoVariations . strictMap . M.fromList
    <$> mapM (treeProd $ readReco dmc) systs

  let eventProd
        = alignThesePipes (\(x, _) (x', _) -> compare x x') truthTree recoTrees

  runEffect $ for eventProd (liftIO . print)

  liftIO . putStrLn $ "closing file " <> fn
  liftIO $ tfileClose tfile


transposeM
  :: forall k k' a. (Ord k, Ord k')
  => M.Map k (M.Map k' a) -> M.Map k' (M.Map k a)
transposeM = M.foldrWithKey f M.empty
  where
    f :: k -> M.Map k' a -> M.Map k' (M.Map k a) -> M.Map k' (M.Map k a)
    f k inmap outmap = M.unionWith (<>) outmap $ M.singleton k <$> inmap
