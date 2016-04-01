{-# LANGUAGE OverloadedStrings, RankNTypes, TupleSections #-}

module Data.Atlas.Tree where

import Data.Semigroup

import Data.ByteString (ByteString)

import qualified Data.Aeson as AT
import qualified Data.Aeson.Types as AT
import Data.Aeson (Value(..), object, FromJSON(..), fromJSON)

import Data.Text (Text)
import qualified Data.Text as T

import Control.Applicative

import Data.Attoparsec.ByteString.Char8


import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Conduit.Attoparsec

import Control.Monad (unless)
import Control.Monad.Catch (MonadThrow(..))

import Data.Atlas.Sample
import Data.Atlas.TopTree


type Weighted a = (Double, a)

parseWeighted :: FromJSON a => [Text] -> Value -> AT.Parser (Double, a)
parseWeighted ws v = do w <- foldr (*) 1.0 <$> mapM (flip parseBranch v) ws
                        (w,) <$> parseJSON v


json' :: (Value -> AT.Parser a) -> Parser a
json' f = AT.json >>= (fromResult . AT.parse f)

json :: FromJSON a => Parser a
json = json' parseJSON


fromResult :: Monad m => AT.Result a -> m a
fromResult (AT.Success y) = return y
fromResult (AT.Error s)   = fail s


branches :: MonadThrow m => Consumer ByteString m [Text]
branches = do
            let p = string "\"branches\"" *> skipSpace *> char ':' *> skipSpace *> json :: Parser [(Text, Text)]
            fmap fst <$> sinkParser p


-- Decode an event as an Aeson Value
event :: MonadThrow m => [Text] -> Consumer ByteString m Value
event brs = object . zip brs <$> sinkParser json


-- TODO
-- we're checking if we're done *every time*
-- could be better...
events :: MonadThrow m => [Text] -> Conduit ByteString m Value
events brs = do done <- sinkParser finished
                unless done $ event brs >>= yield >> sinkParser sep >> events brs

    where
        finished = skipSpace *> ((char ']' *> return True) <|> return False)
        sep = skipSpace *> ((char ',' *> return True) <|> return False)


treeHeader :: MonadThrow m => Consumer ByteString m (Text, [Text])
treeHeader = do title <- sinkParser $ T.pack <$>
                            (char '"' *> manyTill anyChar (char '"'))
                            <* skipSpace <* char ':' <* skipSpace
                            <* char '{' <* skipSpace

                bs <- branches
                _ <- sinkParser $ char ',' <* skipSpace
                                    <* string "\"events\"" <* skipSpace
                                    <* char ':' <* skipSpace
                                    <* char '[' <* skipSpace
                return (title, bs)


treeFooter :: MonadThrow m => Consumer ByteString m ()
treeFooter = sinkParser $ skipSpace <* char '}'


-- TODO
-- currently this does not return the title of the tree...
tree :: (MonadThrow m) => (Value -> AT.Parser e) -> Conduit ByteString m e
tree f = do (_, brs) <- treeHeader
            events brs =$= CL.mapM (fromResult . AT.parse f)
            treeFooter


fileHeader :: MonadThrow m => Consumer ByteString m ()
fileHeader = sinkParser $ skipSpace *> char '{' *> skipSpace


fileFooter :: MonadThrow m => Consumer ByteString m ()
fileFooter = sinkParser $ skipSpace *> char '}' *> skipSpace


comma :: MonadThrow m => Consumer ByteString m ()
comma = sinkParser (skipSpace *> char ',' *> skipSpace)


sampleInfo :: MonadThrow m => Consumer ByteString m SampleInfo
-- TODO
-- SampleInfo should be Monoid?
sampleInfo = tree parseJSON =$= CL.fold (<>) (SampleInfo 0 0 0)


project :: (FromJSON a, MonadThrow m)
          => [Text]
          -> Consumer (Weighted a) m h
          -> Consumer ByteString m (Sample h)
project ws c = do fileHeader
                  s <- sampleInfo
                  comma
                  let ws' = if dsid s == 0 then [] else ws
                  h <- tree (parseWeighted ws') =$= c
                  fileFooter

                  return (s, h)
