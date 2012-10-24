{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

module Data.Text.Rope
  ( CharOffset (..)
  , Rope
  , empty
  , null
  , splitAt
  , take
  , drop
  , concat
  , isPrefixOf
  ) where

import Prelude hiding (splitAt, take, drop, null, concat)
import Data.FingerTree (viewr, ViewR (..), viewl, ViewL (..), Measured (..), (<|), (|>), split)
import qualified Data.FingerTree as FT
import qualified Data.Text as T
import Data.Monoid
import Data.Foldable (toList)

newtype CharOffset = CharOffset { getCharOffset :: Int } deriving (Enum, Real, Ord, Eq, Num, Integral)

type Rope = FT.FingerTree CharOffset T.Text

instance Monoid CharOffset where
  mempty = CharOffset 0
  mappend = (+)

instance Measured CharOffset T.Text where
  measure = CharOffset . T.length

empty :: Rope
empty = FT.empty

null :: Rope -> Bool
null r = case viewl r of
  EmptyL -> True
  x :< xs -> T.null x && null xs

splitAt :: CharOffset -> Rope -> (Rope, Rope)
splitAt n r = case viewr lr of
    EmptyR -> (lr, rr)
    llr :> m -> let (lm, rm) = T.splitAt (fromIntegral $ n - measure lr) m
                in (llr |> lm, rm <| rr)
  where
    (lr, rr) = split (> n) r

take, drop :: CharOffset -> Rope -> Rope
take n = fst . splitAt n
drop n = snd . splitAt n

concat :: Rope -> T.Text
concat = T.concat . toList

isPrefixOf :: T.Text -> Rope -> Bool
text `isPrefixOf` rope = case viewl rope of
  EmptyL -> T.null text
  chunk :< rope' -> if T.length text < T.length chunk
                      then text `T.isPrefixOf` chunk
                      else chunk `T.isPrefixOf` text && (T.drop (T.length chunk) text) `isPrefixOf` rope'