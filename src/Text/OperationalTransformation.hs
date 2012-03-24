{-# LANGUAGE OverloadedStrings #-}

module Text.OperationalTransformation
  ( Action(..)
  , Operation
  , merge
  , apply
  , simplify
  , xform
  ) where

import qualified Data.Text as T
import Data.Monoid (mappend)
import Control.Arrow (first, second, (***))

data Action = Retain Int
            | Insert T.Text
            | Delete T.Text
            deriving (Eq, Read, Show)

type Operation = [Action]

simplify :: Operation -> Operation
simplify ((Retain 0):xs) = simplify xs
simplify ((Insert ""):xs) = simplify xs
simplify ((Delete ""):xs) = simplify xs
simplify ((Retain n):(Retain m):xs) = simplify $ (Retain (n+m)):xs
simplify ((Insert a):(Insert b):xs) = simplify $ (Insert (a `mappend` b)):xs
simplify ((Delete a):(Delete b):xs) = simplify $ (Delete (a `mappend` b)):xs
simplify (x:xs) = x:(simplify xs)
simplify [] = []

haveSamePrefix :: T.Text -> T.Text -> Bool
haveSamePrefix a b = if T.length a < T.length b then a `T.isPrefixOf` b else b `T.isPrefixOf` a

merge :: Operation -> Operation -> Operation
merge [] [] = []
merge ((Retain n):as) ((Retain m):bs) | n == m = (Retain n):(merge as bs)
                                      | n  < m = (Retain n):(merge as ((Retain (m-n)):bs))
                                      | n  > m = (Retain m):(merge ((Retain (n-m)):as) bs)
merge ((Retain n):as) ((Insert b):bs) = (Insert b):(merge ((Retain n):as) bs)
merge ((Retain n):as) ((Delete b):bs) | n == T.length b = (Delete b):(merge as bs)
                                      | n  < T.length b = let (before, after) = T.splitAt n b in (Delete before):(merge as ((Delete after):bs))
                                      | otherwise       = (Delete b):(merge ((Retain (n-(T.length b))):as) bs)
merge ((Insert a):as) ((Retain m):bs) | T.length a == m = (Insert a):(merge as bs)
                                      | T.length a  < m = (Insert a):(merge as ((Retain (m - T.length a)):bs))
                                      | otherwise       = let (before, after) = T.splitAt m a in (Insert before):(merge ((Insert after):as) bs)
merge ((Insert a):as) ((Insert b):bs) = (Insert b):(merge ((Insert a):as) bs)
merge ((Insert a):as) ((Delete b):bs) | not (haveSamePrefix a b) = error "delete must delete what has been inserted before"
                                      | T.length a == T.length b = merge as bs
                                      | T.length a  < T.length b = merge as ((Delete (T.drop (T.length a) b)):bs)
                                      | otherwise                = merge ((Insert (T.drop (T.length b) a)):as) bs
merge ((Delete a):as) ((Retain m):bs) = (Delete a):(merge as ((Retain m):bs))
merge ((Delete a):as) ((Insert b):bs) = (Delete a):(Insert b):(merge as bs)
merge ((Delete a):as) ((Delete b):bs) = (Delete a):(merge as ((Delete b):bs))
merge ((Delete a):as) [] = (Delete a):(merge as [])
merge [] ((Insert b):bs) = (Insert b):(merge [] bs)

apply :: Operation -> T.Text -> T.Text
apply ((Retain n):xs) s = let (before, after) = T.splitAt n s in if T.length before == n then before `mappend` apply xs after else error "does not match"
apply ((Insert i):xs) s = i `mappend` apply xs s
apply ((Delete d):xs) s = if d `T.isPrefixOf` s then apply xs (T.drop (T.length d) s) else error "does not match"
apply [] "" = ""

xform :: Operation -> Operation -> (Operation, Operation)
xform [] [] = ([], [])
xform ((Insert a):as) bs = (***) ((Insert a):) ((Retain $ T.length a):) $ xform as bs
xform as ((Insert b):bs) = (***) ((Retain $ T.length b):) ((Insert b):) $ xform as bs
xform ((Retain n):as) ((Retain m):bs) | n == m = both ((Retain n):) $ xform as bs
                                      | n  < m = both ((Retain n):) $ xform as ((Retain (m-n)):bs)
                                      | n  > m = both ((Retain m):) $ xform ((Retain (n-m)):as) bs
xform ((Delete a):as) ((Delete b):bs) | not (haveSamePrefix a b) = error "two transformations for the same document must delete the same string when they delete something at the same index"
                                      | T.length a == T.length b = xform as bs
                                      | T.length a  < T.length b = xform as ((Delete $ T.drop (T.length a) b):bs)
                                      | otherwise                = xform ((Delete $ T.drop (T.length b) a):as) bs
xform ((Retain n):as) ((Delete b):bs) | n == T.length b = second ((Delete b):) $ xform as bs
                                      | n  < T.length b = let (before, after) = T.splitAt n b in second ((Delete before):) $ xform as ((Delete after):bs)
                                      | otherwise       = second ((Delete b):) $ xform ((Retain (n-(T.length b))):as) bs
xform ((Delete a):as) ((Retain m):bs) | T.length a == m = first ((Delete a):) $ xform as bs
                                      | T.length a  < m = first ((Delete a):) $ xform as ((Retain (m-(T.length a))):bs)
                                      | otherwise       = let (before, after) = T.splitAt m a in first ((Delete before):) $ xform ((Delete after):as) bs

both :: (a -> b) -> (a, a) -> (b, b)
both f = (***) f f
