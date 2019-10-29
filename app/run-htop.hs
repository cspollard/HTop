{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import Prelude hiding (id, (.))
import           Atlas
import           Data.StrictMap
import           Data.StrictHashMap
import qualified Data.Map.Strict as M
import Data.Histogram.Instances
import Data.Profunctor
import Both
import           BFrag.Event
import           BFrag.Model
import           Control.Applicative        (empty)
import           Control.Lens               hiding (each)
import Data.Monoid (First(..))
import           Control.Monad              (when)
import           Control.Monad.Fail
import           Control.Monad.State.Strict
import           Control.Monad.Writer.Class (writer)
import           Data.Bifunctor             (first)
import           Data.List                  (isInfixOf, nub)
import           Data.Maybe                 (fromMaybe)
import           Data.Semigroup hiding (First)
import qualified Data.Text                  as T
import           Data.TFile
import           Data.TTree
import           Data.Typeable
import           GHC.Exts                   (IsList (..))
import qualified Data.Serialize as S
import Data.Serialize.Text
import qualified Data.ByteString.Lazy as BS
import           GHC.Float
import           Options.Generic hiding (First)
import           Pipes
import Control.Category
import           Pipes.Lift
import qualified Pipes.Prelude              as P
import           System.IO                  (BufferMode (..), hSetBuffering,
                                             stdout)


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

  let yummy = foldlMooreK $ fillFile treeSysts
  hs <- runStar (extract . update yummy) fns

  putStrLn ("writing to file " ++ outfile args)
  BS.writeFile (outfile args) . S.encodeLazy $ (fmap.fmap) extract hs  


  where
    chompM m i = runStar (update m) i



type Three a b c = Both a (Both b c)


fillFile
  :: (MonadIO m, MonadCatch m, MonadFail m)
  => [String] -- ^ systematic tree names
  -> MooreK m String (Three (First ProcessInfo) (Sum Double) (Moore' (PhysObj Event) AnaObjs))
fillFile systs =
  feedback . liftMoore (Both mempty (Both mempty eventHs)) . Star $ \(th, fn) -> do
    let Both pi (Both sow aos) = th

    liftIO . putStrLn $ "analyzing file " <> fn

    -- check whether or not this is a data file
    tfile <- tfileOpen fn
    liftIO . putStrLn $ "checking sumWeights"

    tw <- ttree tfile "sumWeights"

    -- partial.
    -- throw an error when there is no dsid.
    (Just (dsidc :: CInt)) <-
      P.head . evalStateP tw $ yield 0 >-> pipeTTree (readBranch "dsid")

    let dsid' = fromEnum dsidc
        procinfo =
          ProcessInfo dsid'
          $ if "_a" `isInfixOf` fn
            then AFII
            else if dsid' == 0 then DS else FS


    liftIO . putStrLn $ "procinfo: " ++ show procinfo

    sow' <-
      fmap float2Double
      . P.fold (+) 0 id
      . evalStateP tw
      $ each ([0..] :: [Int])
        >-> pipeTTree (readBranch "totalEventsWeighted")

    liftIO . putStrLn $ "sum of weights: " ++ show sow'

    let entryFold = P.fold chomps entryMoore id
        entryMoore = feedback $ liftMoore mempty (\(m, (i, j)) -> liftSM (M.insert i j) m)

        entryRead = (,) <$> readRunEventNumber <*> readEntry
        entries t = fmap extract . entryFold . evalStateP t $ allIdxs >-> pipeTTree entryRead
        allIdxs = each ([0..] :: [Int])


    nomTree <- ttree tfile "nominal"
    nomEntries <- entries nomTree

    trueTree <- ttree tfile "particleLevel"
    trueEntries <-
      if dsid' == 0
        then return mempty
        else entries trueTree

    let systflag =
          case dsid' of
            0 -> Data'
            _ ->
              if procinfo `elem` ([nomkey] ++ zjetskeys ++ stopdrkeys ++ dibosonkeys)
                then MC' AllVars
                else MC' NoVars

    (systTrees :: StrictMap String TTree) <-
      case systflag of
        Data' -> return mempty
        MC' NoVars -> return mempty
        MC' AllVars -> fromList <$> mapM (\tn -> (tn,) <$> ttree tfile tn) systs

    systEntries <- mapM entries systTrees

    let allEntries = entryMap trueEntries nomEntries systEntries

    aos' <-
      P.fold chomps aos id
      $ each allEntries
        >-> doEvery 100
            ( \i _ -> liftIO . putStrLn $ show i ++ " entries processed." )
        >-> readEvents systflag trueTree nomTree systTrees
        >-> P.map return

    liftIO . putStrLn $ "closing file " <> fn
    liftIO $ tfileClose tfile
    return . Both (pi <> First (Just procinfo)) $ Both (sow <> Sum sow') aos'

  where
    entryMap
      :: Ord a
      => StrictMap a b
      -> StrictMap a b
      -> StrictMap k (StrictMap a b)
      -> [(a, (Maybe b, Maybe b, StrictMap k (Maybe b)))]
    entryMap trueMap nomMap systMaps =
      fmap (\i -> (i, lookup' i)) . nub
      $ keys' trueMap ++ keys' nomMap ++ foldMap keys' systMaps
      where
        keys' = inSM M.keys
        lookup' i =
          ( trueMap ^? ix i
          , nomMap ^? ix i
          , fmap (^? ix i) systMaps
          )


doEvery :: Monad m => Int -> (Int -> a -> m ()) -> Pipe a a m r
doEvery m f = go 0
  where
    go n = do
      x <- await
      when ((n `mod` m) == 0) (lift $ f n x)
      yield x
      go (n+1)


data TreeReadError = TreeReadError deriving (Typeable, Show)
instance Exception TreeReadError


readEvents
  :: (MonadThrow m, MonadIO m)
  => DataMC'
  -> TTree
  -> TTree
  -> StrictMap String TTree
  -> Pipe ((CUInt, CULong), (Maybe Int, Maybe Int, StrictMap String (Maybe Int))) Event m r
readEvents systflag tttrue ttnom ttsysts = do
  ((rn, en), (mitrue, minom, isysts)) <- await

  -- TODO
  -- fugly
  -- this attempts to read an entry from a tree and returns "empty" if it's not
  -- there
  let f tr t (Just i) =
        fmap (first Just)
        . flip runStateT t
        $ fromMaybe (error "unable to read event from ttree!")
          <$> readTTreeEntry tr i
      f _ t Nothing   = return (Nothing, t)

      g tr t (Just i) =
        flip runStateT t
        $ fromMaybe (error "unable to read event from ttree!")
          <$> readTTreeEntry tr i
      g _ t Nothing   = return (empty, t)

  (true, tttrue') <- f readTrueEvent tttrue mitrue

  let bhs :: [BHadron]
      bhs =
        case true of
          Nothing -> []
          Just (trueEvt, _) ->
            toListOf (trueJets.traverse.tjBHadrons.traverse) trueEvt

      systs' = liftSM2 go ttsysts isysts
        where
          go =
            M.mergeWithKey
              (\_ t mi -> Just $ g (readRecoEvent (MC' NoVars) bhs) t mi)
              (fmap $ const $ throwM TreeReadError)
              (fmap $ const $ throwM TreeReadError)

  (nom, ttnom') <- g (readRecoEvent systflag bhs) ttnom minom
  msysts <- sequence systs'

  -- TODO
  -- traverses Map twice
  let systs = fst <$> msysts
      ttsysts' = snd <$> msysts

  yield
    . Event rn en (maybe empty writer true)
    . toEvent nom
    . fromList
    . fmap (first T.pack)
    $ toList systs

  readEvents systflag tttrue' ttnom' ttsysts'

  where
    toEvent :: PhysObj a -> StrictHashMap T.Text (PhysObj a) -> PhysObj a
    toEvent n s =
      let systs = view nominal . runPhysObj <$> s
          n' = runPhysObj n
          n'' = n' & variations %~ mappend systs
      in physObj n''
