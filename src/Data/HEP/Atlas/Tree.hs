{-# LANGUAGE OverloadedStrings #-}

module Data.HEP.Atlas.Tree where

import Data.ByteString (ByteString(..))
import qualified Data.ByteString.Char8 as BS

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



branches :: MonadThrow m => Sink ByteString m [Text]
branches = do
            let p = string "\"branches\"" *> skipSpace *> char ':' *> skipSpace *> json :: Parser [(Text, Text)]
            fmap fst <$> sinkParser p



-- Decode an event as an Aeson Value
event :: MonadThrow m => [Text] -> Conduit ByteString m Value
event brs = mapOutput (object . zip brs . snd) (conduitParser (json :: Parser [Value]))


-- TODO
-- I don't think this is the "correct" way to do this.
-- should not need await
events :: MonadThrow m => [Text] -> Conduit ByteString m Value
events brs = do
        v <- event brs =$= await
        case v of
            Just v' -> yield v'

        x <- sinkParser sep
        case x of
            True -> events brs
            False -> return ()
    where
        sep = (skipSpace *> char ',' *> skipSpace *> return True) <|> return False


treeHeader :: MonadThrow m => Sink ByteString m (Text, [Text])
treeHeader = do
                title <- sinkParser $ T.pack <$> (char '"' *> manyTill anyChar (char '"')) <* skipSpace <* char ':' <* skipSpace <* char '{' <* skipSpace
                bs <- toConsumer branches
                _ <- sinkParser $ char ',' <* skipSpace
                                    <* string "\"events\"" <* skipSpace
                                    <* char ':' <* skipSpace
                                    <* char '[' <* skipSpace
                return (title, bs)

treeFooter :: MonadThrow m => Sink ByteString m ()
treeFooter = sinkParser $ skipSpace *> char ']' *> skipSpace *> char '}' *> skipSpace

foldTree :: MonadThrow m => (a -> Value -> a) -> a -> Sink ByteString m a
foldTree f x0 = do
        (title, brs) <- toConsumer treeHeader
        x <- events brs =$= CL.fold f x0
        treeFooter >> return x

{-
-- TODO
-- eventually parse entire file
parseFile :: BSL.ByteString -> [(Text, [Value])]
parseFile bs = case parse fileHeader bs of
                    Fail _ _ _ -> []
                    Done bs' _ -> let (x, bs'') = parseTree bs' in 
    where
        fileHeader = skipSpace *> char '{' <* skipSpace
        fileFooter = skipSpace *> char '}' <* skipSpace
-}


