{-# LANGUAGE OverloadedStrings #-}

module Data.HEP.Atlas.TopTree where

import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Map as M

import Control.Applicative

import Data.Text (Text, unpack)
import Data.Aeson (Value(..), withObject, eitherDecodeStrict, object, Result(..), fromJSON)
import Data.Aeson ((.:), FromJSON(..))
import Data.Aeson.Types (Parser, parse)

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Char8 as BS
import qualified Data.Attoparsec.Lazy as AL

import Data.Attoparsec.ByteString.Char8 (skipSpace, char, string, manyTill, takeWhile1, anyChar)

import Data.Monoid ((<>))
import Control.Monad (forM)

import Data.HEP.LorentzVector
import Data.HEP.Atlas.Event
import Data.HEP.Atlas.Electron
import Data.HEP.Atlas.Muon
import Data.HEP.Atlas.Jet

bracketScan :: Char -> Char -> AL.Parser BS.ByteString
bracketScan p q = do
                    _ <- char p
                    mid <- fmap BS.concat . many $ bracketScan p q <|> takeWhile1 isNotBracket
                    _ <- char q
                    return $ (p `BS.cons` mid) `BS.snoc` q
            where isNotBracket c = c /= p && c /= q


-- return event and whether it is the last one
event :: [Text] -> [Text] -> [Text] -> AL.Parser (Event, Bool)
event evtWeights evtSystWeights branches = do
            skipSpace
            evtTxt <- bracketScan '[' ']'
            case eitherDecodeStrict evtTxt of
                Left err -> fail err
                Right evtVals -> case parse (parseEvent evtWeights evtSystWeights) (object (zip branches evtVals)) of
                                        Error s -> fail s
                                        Success evt -> ((,) evt . (/= ',')) <$> (skipSpace *> anyChar )


parseEvents :: [Text] -> [Text] -> [Text] -> BSL.ByteString -> Events
parseEvents evtWeights evtSystWeights branches bs = case AL.parse (event evtWeights evtSystWeights branches) bs of
                    AL.Fail _ _ err -> error err
                    AL.Done bs' (evt, False) -> evt : parseEvents evtWeights evtSystWeights branches bs'
                    AL.Done _ (evt, True) -> [evt]



parseTree :: [Text] -> [Text] -> BSL.ByteString -> Events
parseTree evtWeights evtSystWeights bs = case AL.parse branchesTxt bs of
                AL.Fail _ _ err -> error err
                AL.Done bs' bs'' -> case eitherDecodeStrict bs'' :: Either String [(Text, Text)] of
                                        Left err -> error err
                                        Right branches-> parseEvents evtWeights evtSystWeights (map fst branches) bs'
        where
            headerParse = manyTill anyChar (string "\"events\"") <* skipSpace <* char ':' <* skipSpace <* char '['
            branchesTxt = manyTill anyChar (string "\"branches\"") *> skipSpace *> char ':' *> skipSpace *> bracketScan '[' ']' <* headerParse



parseBranch :: FromJSON a => Text -> Value -> Parser a
parseBranch name = withObject
                        ("parseBranch: the item with key " <> unpack name <> " is not an object.")
                        (.: name)



zipWithA :: (Applicative m) => m ([(a -> b)]) -> m ([a]) -> m ([b])
zipWithA = liftA2 (zipWith ($))


parsePtEtaPhiEs :: Text -> Value -> Parser PtEtaPhiEs
parsePtEtaPhiEs prefix val = fmap PtEtaPhiE `fmap`
                                parseBranch (prefix <> "pt") val `zipWithA`
                                parseBranch (prefix <> "eta") val `zipWithA`
                                parseBranch (prefix <> "phi") val `zipWithA`
                                parseBranch (prefix <> "e") val


parseElectrons :: Value -> Parser Electrons
parseElectrons val = fmap Electron `fmap`
                        parsePtEtaPhiEs "el_" val `zipWithA`
                        parseBranch "el_cl_eta" val `zipWithA`
                        parseBranch "el_charge" val `zipWithA`
                        parseBranch "el_d0sig" val `zipWithA`
                        parseBranch "el_ptvarcone20" val



parseMuons :: Value -> Parser Muons
parseMuons val = fmap Muon `fmap`
                    parsePtEtaPhiEs "mu_" val `zipWithA`
                    parseBranch "mu_charge" val `zipWithA`
                    parseBranch "mu_d0sig" val `zipWithA`
                    parseBranch "mu_ptvarcone30" val


parseJets :: Value -> Parser Jets
parseJets val = fmap Jet `fmap`
                    parsePtEtaPhiEs "jet_" val `zipWithA`
                    parseBranch "jet_mv2c20" val `zipWithA`
                    parseBranch "jet_jvt" val


parseLargeJets :: Value -> Parser LargeJets
parseLargeJets val = fmap LargeJet `fmap`
                            parsePtEtaPhiEs "ljet_" val `zipWithA`
                            parseBranch "ljet_m" val `zipWithA`
                            parseBranch "ljet_sd12" val


parseMET :: Value -> Parser PtEtaPhiE
parseMET val = let et = parseBranch "met_met" val in
                PtEtaPhiE <$>
                    et <*>
                    return 0.0 <*>
                    parseBranch "met_phi" val <*>
                    et


ptSort :: HasLorentzVector v => [v] -> [v]
ptSort = sortBy (comparing (lvPt . toPtEtaPhiE))


parseBranchMap :: FromJSON v => [Text] -> Value -> Parser (M.Map Text v)
parseBranchMap ts v = M.fromList <$> forM ts (\t -> (,) t <$> parseBranch t v)


parseEvent :: [Text] -> [Text] -> Value -> Parser Event
parseEvent evtWeights evtSystWeights v = Event <$>
                    parseBranch "runNumber" v <*>
                    parseBranch "eventNumber" v <*>
                    parseBranch "mcChannelNumber" v <*>
                    parseBranchMap evtWeights v <*>
                    fmap (M.insert "nominal" 1.0) (parseBranchMap evtSystWeights v) <*>
                    parseBranch "mu" v <*>
                    fmap ptSort (parseElectrons v) <*>
                    fmap ptSort (parseMuons v) <*>
                    fmap ptSort (parseJets v) <*>
                    fmap ptSort (parseLargeJets v) <*>
                    parseMET v
