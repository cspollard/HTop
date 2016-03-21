{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Data.HEP.Atlas.Tree where

import Data.ByteString (ByteString)

import qualified Data.Aeson as AT
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
tree :: (MonadThrow m, FromJSON e) => Conduit ByteString m e
tree = do
        (_, brs) <- treeHeader
        events brs =$= CL.mapM (fromResult . fromJSON)
        treeFooter


fileHeader :: MonadThrow m => Consumer ByteString m ()
fileHeader = sinkParser $ skipSpace *> char '{' *> skipSpace


fileFooter :: MonadThrow m => Consumer ByteString m ()
fileFooter = sinkParser $ skipSpace *> char '}' *> skipSpace

comma :: MonadThrow m => Consumer ByteString m ()
comma = sinkParser (skipSpace *> char ',' *> skipSpace)
