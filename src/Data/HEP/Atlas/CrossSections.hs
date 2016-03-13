{-# LANGUAGE OverloadedStrings #-}

module Data.HEP.Atlas.CrossSections where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 hiding (isEndOfLine)
import Data.IntMap
import Data.Either (rights)

isEndOfLine :: Char -> Bool
isEndOfLine c = c == '\n' || c == '\r'

type CrossSectionInfo = IntMap Double

crossSectionInfo :: Parser CrossSectionInfo
crossSectionInfo = fromList . rights <$> (skipSpace *> many (eitherP comment xsecline) <* endOfInput)
                    where
                        comment = char '#' *> takeTill isEndOfLine *> skipSpace
                        xsecline = (,) <$> (decimal <* skipSpace) <*> ((*) <$> (double <* skipSpace) <*> (option 1.0 (double <* skipSpace)))
