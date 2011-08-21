module OperationalTransformation
  ( Action(..)
  , Operation
  , merge
  , apply
  , xform
  ) where

import Data.List (isPrefixOf)
import Control.Arrow (first, second, (***))

data Action = Skip Int
            | Insert String
            | Delete String
            deriving (Show)

type Operation = [Action]

--simplify :: Operation -> Operation
--simplify ((Skip 0):xs) = simplify xs
--simplify ((Insert ""):xs) = simplify xs
--simplify ((Delete ""):xs) = simplify xs
--simplify ((Skip n):(Skip m):xs) = simplify $ (Skip (n+m)):xs
--simplify ((Insert a):(Insert b):xs) = simplify $ (Insert (a++b)):xs
--simplify ((Delete a):(Delete b):xs) = simplify $ (Delete (a++b)):xs
--simplify (x:xs) = x:(simplify xs)
--simplify [] = []

merge :: Operation -> Operation -> Operation
merge [] [] = []
merge ((Skip n):as) ((Skip m):bs) | n == m = (Skip n):(merge as bs)
                                  | n  < m = (Skip n):(merge as ((Skip (m-n)):bs))
                                  | n  > m = (Skip m):(merge ((Skip (n-m)):as) bs)
merge ((Skip n):as) ((Insert b):bs) = (Insert b):(merge ((Skip n):as) bs)
merge ((Skip n):as) ((Delete b):bs) | n == length b = (Delete b):(merge as bs)
                                    | n  < length b = let (before, after) = splitAt n b in (Delete before):(merge as ((Delete after):bs))
                                    | n  > length b = (Delete b):(merge ((Skip (n-(length b))):as) bs)
merge ((Insert a):as) ((Skip m):bs) | length a == m = (Insert a):(merge as bs)
                                    | length a  < m = (Insert a):(merge as ((Skip (m - length a)):bs))
                                    | length a  > m = let (before, after) = splitAt m a in (Insert before):(merge ((Insert after):as) bs)
merge ((Insert a):as) ((Insert b):bs) = (Insert b):(merge ((Insert a):as) bs)
merge ((Insert a):as) ((Delete b):bs) | not . all id $ zipWith (==) a b = error "delete must delete what has been inserted before"
                                      | length a == length b = merge as bs
                                      | length a  < length b = merge as ((Delete (drop (length a) b)):bs)
                                      | length a  > length b = merge ((Insert (drop (length b) a)):as) bs
merge ((Delete a):as) ((Skip m):bs) = (Delete a):(merge as ((Skip m):bs))
merge ((Delete a):as) ((Insert b):bs) = (Delete a):(Insert b):(merge as bs)
merge ((Delete a):as) ((Delete b):bs) = (Delete a):(merge as ((Delete b):bs))
merge ((Delete a):as) [] = (Delete a):(merge as [])
merge [] ((Insert b):bs) = (Insert b):(merge [] bs)

apply :: Operation -> String -> String
apply ((Skip n):xs) s = let (before, after) = splitAt n s in if length before == n then before ++ apply xs after else error "does not match"
apply ((Insert i):xs) s = i ++ apply xs s
apply ((Delete d):xs) s = if d `isPrefixOf` s then apply xs (drop (length d) s) else error "does not match"
apply [] "" = ""

xform :: Operation -> Operation -> (Operation, Operation)
xform [] [] = ([], [])
xform ((Insert a):as) bs = (***) ((Insert a):) ((Skip $ length a):) $ xform as bs
xform as ((Insert b):bs) = (***) ((Skip $ length b):) ((Insert b):) $ xform as bs
xform ((Skip n):as) ((Skip m):bs) | n == m = both ((Skip n):) $ xform as bs
                                  | n  < m = both ((Skip n):) $ xform as ((Skip (m-n)):bs)
                                  | n  > m = both ((Skip m):) $ xform ((Skip (n-m)):as) bs
xform ((Delete a):as) ((Delete b):bs) | not . all id $ zipWith (==) a b = error "two transformations for the same document must delete the same string when they delete something at the same index"
                                      | length a == length b = xform as bs
                                      | length a  < length b = xform as ((Delete $ drop (length a) b):bs)
                                      | length a  > length b = xform ((Delete $ drop (length b) a):as) bs
xform ((Skip n):as) ((Delete b):bs) | n == length b = second ((Delete b):) $ xform as bs
                                    | n  < length b = let (before, after) = splitAt n b in second ((Delete before):) $ xform as ((Delete after):bs)
                                    | n  > length b = second ((Delete b):) $ xform ((Skip (n-(length b))):as) bs
xform ((Delete a):as) ((Skip m):bs) | length a == m = first ((Delete a):) $ xform as bs
                                    | length a  < m = first ((Delete a):) $ xform as ((Skip (m-(length a))):bs)
                                    | length a  > m = let (before, after) = splitAt m a in first ((Delete before):) $ xform ((Delete after):as) bs

both :: (a -> b) -> (a, a) -> (b, b)
both f = (***) f f
