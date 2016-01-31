{-# LANGUAGE OverloadedStrings #-}

module Data.HEP.ATLAS.CrossSections where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.IntMap

type CrossSectionInfo = IntMap Double

crossSectionInfo :: Parser CrossSectionInfo
crossSectionInfo = fromList <$> many p
                    where
                        comment = (char '#' >> takeTill endOfLine) <|> skipSpace
                        xsecline = (,) <$> (scientific << skipSpace) <*> ((*) <$> scientific << skipSpace <*> scientific << skipSpace)
                        p = many comment >> xsecline 
