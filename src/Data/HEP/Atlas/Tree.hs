{-# LANGUAGE OverloadedStrings #-}

module Data.HEP.Atlas.Tree where

import Control.Arrow (first)
import Data.Attoparsec.Lazy (parse, Parser(..), Result(..), choice)

import Data.Attoparsec.ByteString.Char8 (skipSpace, char, string, manyTill, takeWhile1, anyChar, option)

import Control.Applicative (many, (<$>), (<*>), (<|>))

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL

import Data.Text (Text(..), unpack)

import Data.Aeson (Value(..), withObject, object, Result(..), fromJSON, decodeStrict)
import Data.Aeson.Parser (value)

bracketScan :: Char -> Char -> Parser BS.ByteString
bracketScan p q = do
                    _ <- char p
                    mid <- fmap BS.concat . many $ bracketScan p q <|> takeWhile1 isNotBracket
                    _ <- char q
                    return $ (p `BS.cons` mid) `BS.snoc` q
            where isNotBracket c = c /= p && c /= q


branches :: Parser [Text]
branches = do
    b <- go
    case decodeStrict b of
        Nothing -> fail "error parsing branches."
        Just bs -> return bs

    where go = string "\"branches\"" *> skipSpace *> char ':' *> skipSpace *> bracketScan '[' ']'


-- Decode an event as an Aeson Value
event :: [Text] -> Parser Value
event branches = do
                    mv <- fmap (object . zip branches) <$> decodeStrict <$> bracketScan '[' ']'
                    case mv of
                        Nothing -> fail "error parsing branches."
                        Just v -> return v


-- this brings the list-building outside of the (strict) Parser monad
parseMany :: Parser a -> BSL.ByteString -> ([a], BSL.ByteString)
parseMany p bs = case parse p bs of
                    Fail bs' _ _ -> ([], bs')
                    Done bs' x -> first (x:) $ parseMany p bs'


-- get the list of events from a tree
parseTree :: BSL.ByteString -> ([Value], BSL.ByteString)
parseTree bs = case parse (skipSpace *> branches <* skipSpace <* eventsHeader) bs of
                    Fail bs' _ _ -> ([], bs')
                    Done bs' branches -> parseMany (event' branches) bs'

        where
            eventsHeader = do
                            string "\"events\"" <* skipSpace
                            char ':' <* skipSpace
                            char '[' <* skipSpace


            event' brs = do
                            evt <- event brs <* skipSpace
                            choice [char ',', char ']']
                            return evt
