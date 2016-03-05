{-# LANGUAGE OverloadedStrings #-}

module Data.HEP.Atlas.Tree where

import Data.ByteString (ByteString(..))
import qualified Data.ByteString.Char8 as BS

import Data.Text (Text(..))
import qualified Data.Text as T

import Control.Applicative

import Data.Attoparsec.ByteString.Char8 -- (parse, Parser(..), Result(..), eitherResult, char, string, skipSpace, takeWhile1)

import Data.Aeson (decodeStrict, Value, object)

import Data.Conduit
import Data.Conduit.Attoparsec

import Control.Monad.Catch (MonadThrow(..))

bracketScan :: MonadThrow m => Char -> Char -> Sink ByteString m ByteString
bracketScan p q = sinkParser go
            where
                go = do
                        char p
                        mid <- fmap BS.concat . many $ go <|> takeWhile1 isNotBracket
                        char q
                        return $ (p `BS.cons` mid) `BS.snoc` q

                isNotBracket c = c /= p && c /= q


branches :: MonadThrow m => Sink ByteString m [Text]
branches = do
            _ <- sinkParser $ string "\"branches\"" *> skipSpace *> char ':' *> skipSpace *> return []
            bs <- bracketScan '[' ']'
            case decodeStrict bs of
                Nothing -> fail "error parsing branches."
                Just bs -> return $ (fmap head) bs



-- Decode an event as an Aeson Value
event :: MonadThrow m => [Text] -> Conduit ByteString m (Maybe Value)
event bs = yield =<< fmap (object . zip bs) <$> decodeStrict <$> toConsumer (bracketScan '[' ']')

-- TODO
-- currently does not return the title of the tree.
-- get the list of events from a tree
parseTree :: MonadThrow m => Conduit ByteString m (Maybe Value)
parseTree = do
                title <- sinkParser $ T.pack <$> (char '"' *> manyTill anyChar (char '"')) <* skipSpace <* char ':' <* skipSpace <* char '{' <* skipSpace
                bs <- toConsumer (branches)
                event bs

                {-
                    Fail bs' ss s -> traceShow (s, ss) $ (Nothing, bs')
                    Done bs' (title, branches) ->
                                let (evts, bs'') =  parseMany (event' branches) bs' in
                                    (Just (title, evts), bs'')

        where
            header = do
                        title <- pack <$> (char '"' *> manyTill anyChar (char '"'))
                        skipSpace <* char ':' <* skipSpace <* char '{'
                        brs <- skipSpace *> branches <* skipSpace <* eventsHeader
                        return (title, brs)

            eventsHeader = do
                            char ',' *> skipSpace *> string "\"events\"" <* skipSpace
                            char ':' <* skipSpace
                            char '[' <* skipSpace


            event' brs = do
                            evt <- event brs <* skipSpace
                            -- TODO
                            -- this is not very precise
                            -- we can end the tree dictionary here!
                            choice [char ',' <* skipSpace,
                                    char ']' <* skipSpace <* char '}' <* skipSpace <* option ',' (char ',' <* skipSpace)]
                            return evt
                            -}

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

