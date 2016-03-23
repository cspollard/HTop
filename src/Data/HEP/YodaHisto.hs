{-# LANGUAGE OverloadedStrings, DeriveGeneric, TypeFamilies #-}

module Data.HEP.YodaHisto where

import Data.Maybe (fromJust)
import Data.Semigroup

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Data.Serialize (Serialize(..))
import GHC.Generics (Generic)

import Data.Histogram

instance Serialize Text where
    put = put . T.encodeUtf8
    get = do x <- get
             case x of
                Left err -> fail err
                Right y  -> return y


-- a YodaHisto is just a histogram with some annotations.
data YodaHisto b val = YodaHisto { path :: Text
                                 , xLabel :: Text
                                 , yLabel :: Text
                                 , yhHisto :: !(Histogram b val)
                                 } deriving (Generic, Show)

type YodaHisto1D = YodaHisto (Bin1D Double) (Dist1D Double)

instance (Serialize val, Serialize b) => Serialize (YodaHisto b val) where

alterHisto :: (Histogram b val -> Histogram b val') -> YodaHisto b val -> YodaHisto b val'
alterHisto f (YodaHisto p xl yl h) = YodaHisto p xl yl $ f h

instance Functor (YodaHisto b) where
    -- fmap f (YodaHisto p xl yl h) = YodaHisto p xl yl $ fmap f h
    fmap f = alterHisto (fmap f)

instance ScaleW val => ScaleW (YodaHisto b val) where
    type W (YodaHisto b val) = W val
    yh `scaleW` w = (`scaleW` w) `alterHisto` yh

instance (Distribution val, Bin b, BinValue b ~ X val) => Distribution (YodaHisto b val) where
    type X (YodaHisto b val) = X val
    yh `fill` (w, x) = flip fill (w, x) `alterHisto` yh

-- TODO
-- this is bad.
instance (Eq b, Semigroup val) => Semigroup (YodaHisto b val) where
    (<>) = haddUnsafe


xlPrefix :: Text -> YodaHisto b val -> YodaHisto b val
xlPrefix pre (YodaHisto p xl yl h) = YodaHisto p (pre <> xl) yl h

ylPrefix :: Text -> YodaHisto b val -> YodaHisto b val
ylPrefix pre (YodaHisto p xl yl h) = YodaHisto p xl (pre <> yl) h

pathPrefix :: Text -> YodaHisto b val -> YodaHisto b val
pathPrefix pre (YodaHisto p xl yl h) = YodaHisto (pre <> p) xl yl h

-- TODO
-- generalize
-- unsafe!
haddUnsafe :: (Eq b, Semigroup val) => YodaHisto b val -> YodaHisto b val -> YodaHisto b val
haddUnsafe (YodaHisto _ _ _ h) (YodaHisto p xl yl h') = YodaHisto p xl yl (fromJust $ h `hadd` h')

showHisto :: YodaHisto1D -> Text
showHisto (YodaHisto p xl yl h) = T.unlines $
                            [ "# BEGIN YODA_HISTO1D " <> p, "Path=" <> p, "Type=Histo1D"
                            , "XLabel=" <> xl, "YLabel=" <> yl
                            , "Total\tTotal\t" <> distToText (integral h)
                            , "Underflow\tUnderflow\t" <> distToText (underflow h)
                            , "Overflow\tOverflow\t" <> distToText (overflow h)
                            ] ++
                            map (\((Z :. xmin, Z :. xmax), b) -> T.pack (show xmin ++ "\t" ++ show xmax ++ "\t") <> distToText b) (toTuples h) ++
                            [ "# END YODA_HISTO1D", "" ]

                            where
                                distToText (Dist0 sw sw2 ne :. DistWX swx swx2) = T.pack $
                                                show sw ++ "\t" ++
                                                show sw2 ++ "\t" ++
                                                show swx ++ "\t" ++
                                                show swx2 ++ "\t" ++
                                                show ne
