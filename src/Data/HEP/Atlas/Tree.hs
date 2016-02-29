{-# LANGUAGE OverloadedStrings #-}

module Data.HEP.Atlas.Tree where

import Data.Conduit
import Data.Conduit.Attoparsec

import Data.Attoparsec.Types (Parser)

import Data.Attoparsec.ByteString.Char8 (skipSpace, char, string, manyTill, takeWhile1, anyChar)

import Control.Applicative (many, (<$>), (<*>), (<|>))

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL

import Data.Text (Text(..), unpack)

import Data.Aeson (Value(..), withObject, object, Result(..), fromJSON, decodeStrict)
import Data.Aeson.Parser (value)

bracketScan :: Char -> Char -> Parser BS.ByteString BS.ByteString
bracketScan p q = do
                    _ <- char p
                    mid <- fmap BS.concat . many $ bracketScan p q <|> takeWhile1 isNotBracket
                    _ <- char q
                    return $ (p `BS.cons` mid) `BS.snoc` q
            where isNotBracket c = c /= p && c /= q


branches :: Monad m => Conduit BS.ByteString m (Either ParseError (PositionRange, [Text]))
branches = conduitParserEither $ do
    b <- go
    case decodeStrict b of
        Nothing -> fail "error parsing branches."
        Just bs -> return bs

    where go = string "\"branches\"" *> skipSpace *> char ':' *> skipSpace *> bracketScan '[' ']'


-- Decode an event as an Aeson Value
event :: Monad m =>
            [Text] -> Conduit BS.ByteString m (Either ParseError (PositionRange, Value))
event branches = conduitParserEither $ do
                    mv <- fmap (object . zip branches) <$> decodeStrict <$> bracketScan '[' ']'
                    case mv of
                        Nothing -> fail "error parsing branches."
                        Just v -> return v


parseTree :: Monad m => Conduit BS.ByteString m (Either ParseError (PositionRange, [Value]))
parseTree = conduitParserEither $ do
                branches <- decodeStrict <$> parseBranches

                case branches of
                    Fail bs' _ _ -> ([], bs')
                    Done bs' Nothing -> ([], bs')
                    Done bs' (Just branches) -> parseMany (event branches <* skipSpace) bs'

        where
            parseBranches = char '{' *> skipSpace *> 
-}
