{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses #-}

module Text.OperationalTransformation
  ( Action (..)
  , TextOperation (..)
  ) where

import Control.OperationalTransformation
import qualified Data.Text as T
import Data.Monoid (mappend)

data Action = Retain Int
            | Insert T.Text
            | Delete T.Text
            deriving (Eq, Read, Show)

newtype TextOperation = TextOperation [Action] deriving (Eq, Read, Show)

haveSamePrefix :: T.Text -> T.Text -> Bool
haveSamePrefix a b = if T.length a < T.length b then a `T.isPrefixOf` b
                                                else b `T.isPrefixOf` a

addRetain :: Int -> [Action] -> [Action]
addRetain n (Retain m : xs) = Retain (n+m) : xs
addRetain n xs = Retain n : xs

addInsert :: T.Text -> [Action] -> [Action]
addInsert s (Insert t : xs) = Insert (t `mappend` s) : xs
addInsert s xs = Insert s : xs

addDelete :: T.Text -> [Action] -> [Action]
addDelete s (Delete t : xs) = Delete (t `mappend` s) : xs
addDelete s xs = Delete s : xs

instance OTOperation TextOperation where
  transform (TextOperation o1) (TextOperation o2) = both (TextOperation . reverse) `fmap` loop o1 o2 [] []
    where
      both :: (a -> b) -> (a, a) -> (b, b)
      both f (a, b) = (f a, f b)

      loop [] [] xs ys = Just (xs, ys)
      loop aa@(a:as) bb@(b:bs) xs ys = case (a, b) of
        (Insert i, _) -> loop as bb (addInsert i xs) (addRetain (T.length i) ys)
        (_, Insert i) -> loop aa bs (addRetain (T.length i) xs) (addInsert i ys)
        (Retain n, Retain m) -> case compare n m of
          LT -> loop as (Retain (m-n) : bs) (addRetain n xs) (addRetain n ys)
          EQ -> loop as bs (addRetain n xs) (addRetain n ys)
          GT -> loop (Retain (n-m) : as) bs (addRetain m xs) (addRetain m ys)
        (Delete d, Delete e) -> if not (haveSamePrefix d e)
          then Nothing
          else case compare (T.length d) (T.length e) of
            LT -> loop as (Delete (T.drop (T.length d) e) : bs) xs ys
            EQ -> loop as bs xs ys
            GT -> loop (Delete (T.drop (T.length e) d) : as) bs xs ys
        (Retain r, Delete d) -> case compare r (T.length d) of
          LT -> let (before, after) = T.splitAt r d
                in loop as (Delete after : bs) xs (addDelete before ys)
          EQ -> loop as bs xs (addDelete d ys)
          GT -> loop (Retain (r - T.length d) : as) bs xs (addDelete d ys)
        (Delete d, Retain r) -> case compare (T.length d) r of
          LT -> loop as (Retain (r - T.length d) : bs) (addDelete d xs) ys
          EQ -> loop as bs (addDelete d xs) ys
          GT -> let (before, after) = T.splitAt r d
                in loop (Delete after : as) bs (addDelete before xs) ys
      loop [] (Insert i : bs) xs ys = loop [] bs (addRetain (T.length i) xs) (addInsert i ys)
      loop (Insert i : as) [] xs ys = loop as [] (addInsert i xs) (addRetain (T.length i) ys)
      loop _ _ _ _ = Nothing

instance OTComposableOperation TextOperation where
  compose (TextOperation o1) (TextOperation o2) = (TextOperation . reverse) `fmap` loop o1 o2 []
    where
      loop [] [] xs = Just xs
      loop aa@(a:as) bb@(b:bs) xs = case (a, b) of
        (Delete d, _) -> loop as bb (addDelete d xs)
        (_, Insert i) -> loop aa bs (addInsert i xs)
        (Retain n, Retain m) -> case compare n m of
          LT -> loop as (Retain (m-n) : bs) (addRetain n xs)
          EQ -> loop as bs (addRetain n xs)
          GT -> loop (Retain (n-m) : as) bs (addRetain m xs)
        (Retain n, Delete d) -> case compare n (T.length d) of
          LT -> let (before, after) = T.splitAt n d
                in loop as (Delete after : bs) (addDelete before xs)
          EQ -> loop as bs (addDelete d xs)
          GT -> loop (Retain (n - T.length d) : as) bs (addDelete d xs)
        (Insert i, Retain m) -> case compare (T.length i) m of
          LT -> loop as (Retain (m - T.length i) : bs) (addInsert i xs)
          EQ -> loop as bs (addInsert i xs)
          GT -> let (before, after) = T.splitAt m i
                in loop (Insert after : as) bs (addInsert before xs)
        (Insert i, Delete d) -> if not (haveSamePrefix i d)
          then Nothing
          else case compare (T.length i) (T.length d) of
            LT -> loop as (Delete (T.drop (T.length i) d) : bs) xs
            EQ -> loop as bs xs
            GT -> loop (Insert (T.drop (T.length d) i) : as) bs xs
      loop (Delete d : as) [] xs = loop as [] (addDelete d xs)
      loop [] (Insert i : bs) xs = loop [] bs (addInsert i xs)
      loop _ _ _ = Nothing

instance OTSystem T.Text TextOperation where
  apply (TextOperation actions) input = loop actions input ""
    where
      loop [] "" ot = Just ot
      loop (op:ops) it ot = case op of
        Retain r -> if T.length it < r
          then Nothing
          else let (before, after) = T.splitAt r it
               in loop ops after (ot `mappend` before)
        Insert i -> loop ops it (ot `mappend` i)
        Delete d -> if not (d `T.isPrefixOf` it)
          then Nothing
          else loop ops (T.drop (T.length d) it) ot
      loop _ _ _ = Nothing
