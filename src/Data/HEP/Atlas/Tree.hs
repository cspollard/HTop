{-# LANGUAGE OverloadedStrings #-}

module Data.HEP.Atlas.Tree where

import Data.ByteString (ByteString(..))
import qualified Data.ByteString.Char8 as BS

import Data.Maybe (fromJust)

import qualified Data.Aeson as AT

import Data.Text (Text(..))
import qualified Data.Text as T

import Control.Applicative

import Data.Attoparsec.ByteString.Char8 -- (parse, Parser(..), Result(..), eitherResult, char, string, skipSpace, takeWhile1)

import Data.Aeson (decodeStrict, Value, object)

import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Conduit.Attoparsec

import Control.Monad.Catch (MonadThrow(..))

import Debug.Trace


json :: AT.FromJSON a => Parser a
json = do
                x <- AT.json
                case AT.fromJSON x of
                    AT.Success y -> return y
                    AT.Error   s -> fail s



branches :: MonadThrow m => Consumer ByteString m [Text]
branches = do
            let p = string "\"branches\"" *> skipSpace *> char ':' *> skipSpace *> json :: Parser [(Text, Text)]
            fmap fst <$> sinkParser p


-- Decode an event as an Aeson Value
event :: MonadThrow m => [Text] -> Conduit ByteString m Value
event brs = (object . zip brs . snd) `mapOutput` c
        -- this is just to force event to yield one event at a time.
        -- should fail if not Just x (i.e. there is no more input)
        where c = yield =<< (fromJust <$> (conduitParser json =$= await))


events :: MonadThrow m => [Text] -> Conduit ByteString m Value
events brs = do
        event brs
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
tree :: MonadThrow m => Conduit ByteString m Value
tree = do
        (title, brs) <- treeHeader
        events brs
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

-- TODO
-- currently this just loops over trees and yields all events
file :: MonadThrow m => Conduit ByteString m Value
file = do
        sinkParser fileHeader
        trees
        sinkParser fileFooter
        return ()

    where
        fileHeader = skipSpace *> char '{' <* skipSpace
        fileFooter = skipSpace *> char '}' <* skipSpace
