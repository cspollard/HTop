{-# LANGUAGE OverloadedStrings #-}

module Data.HEP.Atlas.Tree where

import qualified Data.Attoparsec.Lazy as AL
import Control.Arrow (first)

import Data.Attoparsec.ByteString.Char8 (skipSpace, char, string, manyTill, takeWhile1, anyChar)

import Control.Applicative (many, (<$>), (<*>), (<|>))

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL

import Data.Text (Text(..), unpack)

import Data.Aeson (Value(..), withObject, object, Result(..), fromJSON, decodeStrict)
import Data.Aeson.Parser (value)

bracketScan :: Char -> Char -> AL.Parser BS.ByteString
bracketScan p q = do
                    _ <- char p
                    mid <- fmap BS.concat . many $ bracketScan p q <|> takeWhile1 isNotBracket
                    _ <- char q
                    return $ (p `BS.cons` mid) `BS.snoc` q
            where isNotBracket c = c /= p && c /= q


event :: [Text] -> AL.Parser Value
event branches = do
                    mv <- fmap (object . zip branches) <$> decodeStrict <$> bracketScan '[' ']'
                    case mv of
                        Nothing -> fail "error parsing branches."
                        Just v -> return v


parseMany :: AL.Parser a -> BSL.ByteString -> ([a], BSL.ByteString)
parseMany p bs = case AL.parse p $ bs of
                    AL.Done bs' x -> first (x :) $ parseMany p bs'
                    AL.Fail bs' _ _ -> ([], bs')

parseTree :: BSL.ByteString -> ([Value], BSL.ByteString)
parseTree bs = case decodeStrict <$> AL.parse parseBranches bs of
            AL.Fail bs' _ _ -> ([], bs')
            AL.Done bs' Nothing -> ([], bs')
            AL.Done bs' (Just branches) -> parseMany (event branches <* skipSpace) bs'

        where
            parseBranches = char '{' *> manyTill anyChar (string "\"branches\"") *>
                skipSpace *> char ':' *> skipSpace *> bracketScan '[' ']'

