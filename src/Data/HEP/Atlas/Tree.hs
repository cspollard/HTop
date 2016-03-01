{-# LANGUAGE OverloadedStrings #-}

module Data.HEP.Atlas.Tree where

import Control.Arrow (first)
import Data.Attoparsec.Lazy (parse, Parser(..), Result(..), choice, (<?>))

import Data.Attoparsec.ByteString.Char8 (skipSpace, char, string, manyTill, takeWhile1, anyChar, option)

import Control.Applicative (many, (<$>), (<*>), (<|>))

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL

import Data.Text (Text(..), pack)

import Data.Aeson (Value(..), withObject, object, Result(..), fromJSON, decodeStrict)
import qualified Data.Aeson.Types as AT
import Data.Aeson.Parser (value)


import Debug.Trace (traceShow, traceShowId)

bracketScan :: Char -> Char -> Parser BS.ByteString
bracketScan p q = do
                    _ <- char p
                    mid <- fmap BS.concat . many $ bracketScan p q <|> takeWhile1 isNotBracket
                    _ <- char q
                    return $ (p `BS.cons` mid) `BS.snoc` q
            where isNotBracket c = c /= p && c /= q


branches :: Parser [Text]
branches = named "branches" $ do
    b <- go
    case decodeStrict b of
        Nothing -> fail "error parsing branches."
        Just bs -> return $ (fmap head) bs

    where go = string "\"branches\"" *> skipSpace *> char ':' *> skipSpace *> bracketScan '[' ']'


-- Decode an event as an Aeson Value
event :: [Text] -> Parser Value
event bs = named "event" $ do
                    mv <- fmap (object . zip bs) <$> decodeStrict <$> bracketScan '[' ']'
                    case mv of
                        Nothing -> fail "error parsing event/branches."
                        Just v -> return v


-- this brings the list-building outside of the (strict) Parser monad
parseMany :: Parser a -> BSL.ByteString -> ([a], BSL.ByteString)
parseMany p bs = case parse p bs of
                    Fail bs' _ _ -> ([], bs')
                    Done bs' x -> first (x:) $ parseMany p bs'


named :: String -> Parser a -> Parser a
named s p = p <?> s

-- get the list of events from a tree
parseTree :: BSL.ByteString -> (Maybe (Text, [Value]), BSL.ByteString)
parseTree bs = case parse header bs of
                    Fail bs' ss s -> traceShow (s, ss) $ (Nothing, bs')
                    Done bs' (title, branches) ->
                                let (evts, bs'') =  parseMany (event' branches) bs' in
                                    (Just (title, evts), bs'')

        where
            header = named "header" $ do
                        title <- pack <$> (char '"' *> manyTill anyChar (char '"'))
                        skipSpace <* char ':' <* skipSpace <* char '{'
                        brs <- skipSpace *> branches <* skipSpace <* eventsHeader
                        return (title, brs)

            eventsHeader = named "eventsHeader" $ do
                            char ',' *> skipSpace *> string "\"events\"" <* skipSpace
                            char ':' <* skipSpace
                            char '[' <* skipSpace


            event' brs = named "event'" $ do
                            evt <- event brs <* skipSpace
                            -- TODO
                            -- this is not very precise
                            -- we can end the tree dictionary here!
                            choice [char ',' <* skipSpace,
                                    char ']' <* skipSpace <* char '}' <* skipSpace <* option ',' (char ',' <* skipSpace)]
                            return evt


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
