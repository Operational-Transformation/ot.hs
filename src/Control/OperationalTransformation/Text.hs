{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}

module Control.OperationalTransformation.Text
  ( Action (..)
  , TextOperation (..)
  , invertOperation
  ) where

import Control.OperationalTransformation
import qualified Data.Text as T
import Data.Monoid (mappend)
import Data.Aeson (Value (..), FromJSON (..), ToJSON (..))
import Data.Attoparsec.Number (Number (..))
import Data.Typeable (Typeable)

data Action = Retain !Int
            | Insert !T.Text
            | Delete !Int
            deriving (Eq, Read, Show, Typeable)

instance ToJSON Action where
  toJSON (Retain n) = Number $ I (toInteger n)
  toJSON (Insert t) = String t
  toJSON (Delete n) = Number $ I (toInteger (-n))

instance FromJSON Action where
  parseJSON (Number (I n)) | n > 0 = return $ Retain (fromInteger n)
                           | n < 0 = return $ Delete (fromInteger (-n))
  parseJSON (String i) = return $ Insert i
  parseJSON _ = fail "expected a non-zero integer or a string"

newtype TextOperation = TextOperation [Action] deriving (Eq, Read, Show, Typeable, FromJSON, ToJSON)

addRetain :: Int -> [Action] -> [Action]
addRetain n (Retain m : xs) = Retain (n+m) : xs
addRetain n xs = Retain n : xs

addInsert :: T.Text -> [Action] -> [Action]
addInsert s (Insert t : xs) = Insert (t `mappend` s) : xs
addInsert s xs = Insert s : xs

addDelete :: Int -> [Action] -> [Action]
addDelete n (Delete m : xs) = Delete (n+m) : xs
addDelete n xs = Delete n : xs

instance OTOperation TextOperation where
  transform (TextOperation o1) (TextOperation o2) = both (TextOperation . reverse) `fmap` loop o1 o2 [] []
    where
      both :: (a -> b) -> (a, a) -> (b, b)
      both f (a, b) = (f a, f b)

      loop [] [] xs ys = Right (xs, ys)
      loop aa@(a:as) bb@(b:bs) xs ys = case (a, b) of
        (Insert i, _) -> loop as bb (addInsert i xs) (addRetain (T.length i) ys)
        (_, Insert i) -> loop aa bs (addRetain (T.length i) xs) (addInsert i ys)
        (Retain n, Retain m) -> case compare n m of
          LT -> loop as (Retain (m-n) : bs) (addRetain n xs) (addRetain n ys)
          EQ -> loop as bs (addRetain n xs) (addRetain n ys)
          GT -> loop (Retain (n-m) : as) bs (addRetain m xs) (addRetain m ys)
        (Delete n, Delete m) -> case compare n m of
          LT -> loop as (Delete (m-n) : bs) xs ys
          EQ -> loop as bs xs ys
          GT -> loop (Delete (n-m) : as) bs xs ys
        (Retain r, Delete d) -> case compare r d of
          LT -> loop as (Delete (d-r) : bs) xs (addDelete r ys)
          EQ -> loop as bs xs (addDelete d ys)
          GT -> loop (Retain (r-d) : as) bs xs (addDelete d ys)
        (Delete d, Retain r) -> case compare d r of
          LT -> loop as (Retain (r-d) : bs) (addDelete d xs) ys
          EQ -> loop as bs (addDelete d xs) ys
          GT -> loop (Delete (d-r) : as) bs (addDelete r xs) ys
      loop [] (Insert i : bs) xs ys = loop [] bs (addRetain (T.length i) xs) (addInsert i ys)
      loop (Insert i : as) [] xs ys = loop as [] (addInsert i xs) (addRetain (T.length i) ys)
      loop _ _ _ _ = Left "the operations couldn't be transformed because they haven't been applied to the same document"

instance OTComposableOperation TextOperation where
  compose (TextOperation o1) (TextOperation o2) = (TextOperation . reverse) `fmap` loop o1 o2 []
    where
      loop [] [] xs = Right xs
      loop aa@(a:as) bb@(b:bs) xs = case (a, b) of
        (Delete d, _) -> loop as bb (addDelete d xs)
        (_, Insert i) -> loop aa bs (addInsert i xs)
        (Retain n, Retain m) -> case compare n m of
          LT -> loop as (Retain (m-n) : bs) (addRetain n xs)
          EQ -> loop as bs (addRetain n xs)
          GT -> loop (Retain (n-m) : as) bs (addRetain m xs)
        (Retain r, Delete d) -> case compare r d of
          LT -> loop as (Delete (d-r) : bs) (addDelete r xs)
          EQ -> loop as bs (addDelete d xs)
          GT -> loop (Retain (r-d) : as) bs (addDelete d xs)
        (Insert i, Retain m) -> case compare (T.length i) m of
          LT -> loop as (Retain (m - T.length i) : bs) (addInsert i xs)
          EQ -> loop as bs (addInsert i xs)
          GT -> let (before, after) = T.splitAt m i
                in loop (Insert after : as) bs (addInsert before xs)
        (Insert i, Delete d) -> case compare (T.length i) d of
          LT -> loop as (Delete (d - T.length i) : bs) xs
          EQ -> loop as bs xs
          GT -> loop (Insert (T.drop d i) : as) bs xs
      loop (Delete d : as) [] xs = loop as [] (addDelete d xs)
      loop [] (Insert i : bs) xs = loop [] bs (addInsert i xs)
      loop _ _ _ = Left "the operations couldn't be composed since their lengths don't match"

instance OTSystem T.Text TextOperation where
  apply (TextOperation actions) input = loop actions input ""
    where
      loop [] "" ot = Right ot
      loop (op:ops) it ot = case op of
        Retain r -> if T.length it < r
          then Left "operation can't be applied to the document: operation is longer than the text"
          else let (before, after) = T.splitAt r it
               in loop ops after (ot `mappend` before)
        Insert i -> loop ops it (ot `mappend` i)
        Delete d -> if d > T.length it
          then Left "operation can't be applied to the document: operation is longer than the text"
          else loop ops (T.drop d it) ot
      loop _ _ _ = Left "operation can't be applied to the document: text is longer than the operation"

invertOperation :: TextOperation -> T.Text -> Either String TextOperation
invertOperation (TextOperation actions) doc = loop actions doc []
  where
    loop (op:ops) text inv = case op of
      (Retain n) -> loop ops (T.drop n text) (Retain n : inv)
      (Insert i) -> loop ops text (Delete (T.length i) : inv)
      (Delete d) -> let (before, after) = T.splitAt d text
                    in loop ops after (Insert before : inv)
    loop [] "" inv = Right . TextOperation . reverse $ inv
    loop [] _ _ = Left "invert failed: text is longer than the operation"