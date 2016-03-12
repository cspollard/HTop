{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Data.HEP.Atlas.Tree where

import Data.Monoid ((<>))

import Data.ByteString (ByteString(..))
import qualified Data.ByteString.Char8 as BS

import Data.Maybe (fromJust)

import qualified Data.Aeson as AT
import Data.Aeson (Value(..), object, FromJSON(..), fromJSON)

import Data.Text (Text(..))
import qualified Data.Text as T

import Control.Applicative

import Data.Attoparsec.ByteString.Char8 -- (parse, Parser(..), Result(..), eitherResult, char, string, skipSpace, takeWhile1)


import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Conduit.Attoparsec

import Control.Monad.Catch (MonadThrow(..))


import Data.HEP.Atlas.Sample


json :: AT.FromJSON a => Parser a
json = AT.json >>= (fromResult . fromJSON)


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


events :: MonadThrow m => [Text] -> Conduit ByteString m Value
events brs = do
        yield =<< event brs
        x <- sinkParser sep
        case x of
            True -> events brs
            False -> return ()
    where
        sep = (skipSpace *> char ',' *> skipSpace *> return True) <|> return False


treeHeader :: MonadThrow m => Consumer ByteString m (Text, [Text])
treeHeader = do
                title <- sinkParser $ T.pack <$>
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
treeFooter = sinkParser $ skipSpace *> char ']' *> skipSpace *> char '}' *> skipSpace


-- TODO
-- currently this does not return the title of the tree...
tree :: (MonadThrow m, FromJSON e) => Conduit ByteString m e
tree = do
        (title, brs) <- treeHeader
        events brs =$= CL.mapM (fromResult . fromJSON)
        treeFooter


trees :: MonadThrow m => Conduit ByteString m Value
trees = do
        tree
        x <- sinkParser sep
        case x of
            True -> trees
            False -> return ()
    where
        sep = (skipSpace *> char ',' *> skipSpace *> return True) <|> return False


fileHeader :: MonadThrow m => Consumer ByteString m ()
fileHeader = sinkParser $ skipSpace *> char '{' *> skipSpace


fileFooter :: MonadThrow m => Consumer ByteString m ()
fileFooter = sinkParser $ skipSpace *> char '}' *> skipSpace

comma :: MonadThrow m => Consumer ByteString m ()
comma = sinkParser (skipSpace *> char ',' *> skipSpace)
