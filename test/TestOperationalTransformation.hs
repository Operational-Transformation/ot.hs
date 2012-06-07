{-# LANGUAGE OverloadedStrings #-}

import Control.OperationalTransformation
import Text.OperationalTransformation

import Test.QuickCheck
import qualified Data.Text as T
import Data.String (fromString)
import Data.Maybe (fromJust)

import Control.Monad (liftM, liftM2)

instance Arbitrary Action where
  arbitrary = oneof [ liftM (Retain . (+1) . abs) arbitrary
                    , liftM Insert arbitrary
                    , liftM Delete arbitrary
                    ]

data TextOperationPair = SOP T.Text TextOperation deriving (Show)

instance Arbitrary TextOperationPair where
  arbitrary = do
    text <- arbitrary
    operation <- arbitraryOperation text
    return $ SOP text operation

-- | has at least 1 element
arbitraryList1 :: (Arbitrary a) => Gen [a]
arbitraryList1 = liftM2 (:) arbitrary arbitrary

arbitraryOperation :: T.Text -> Gen TextOperation
arbitraryOperation = liftM TextOperation . gen
  where
    gen "" = oneof [return [], liftM ((:[]) . Insert . fromString) arbitraryList1]
    gen s = do
      len <- choose (1, min 10 (T.length s))
      oneof [ liftM (Retain len :) $ gen (T.drop len s)
            , do s2 <- liftM fromString arbitraryList1 -- make sure that the text has a length of at least one
                 next <- gen s
                 return $ (Insert s2):next
            , do let (before, after) = T.splitAt len s
                 next <- gen after
                 return $ (Delete before):next
            ]

data TextOperationTriple = SOT T.Text TextOperation TextOperation deriving (Show)

instance Arbitrary TextOperationTriple where
  arbitrary = do
    (SOP text operation1) <- arbitrary
    operation2 <- arbitraryOperation (fromJust $ apply operation1 text)
    return $ SOT text operation1 operation2

data TextConcurrentOperationTriple = SCOT T.Text TextOperation TextOperation deriving (Show)

instance Arbitrary T.Text where
  arbitrary = liftM fromString arbitrary

instance Arbitrary TextConcurrentOperationTriple where
  arbitrary = do
    text <- arbitrary
    operationA <- arbitraryOperation text
    operationB <- arbitraryOperation text
    return $ SCOT text operationA operationB

deltaLength :: TextOperation -> Int
deltaLength (TextOperation ops) = sum (map len ops)
  where len (Retain _)   = 0
        len (Insert i) = T.length i
        len (Delete d) = -(T.length d)

prop_length :: TextOperationPair -> Bool
prop_length (SOP str op) = case apply op str of
  Nothing -> False
  Just str' -> T.length str' == T.length str + deltaLength op

prop_compose_length :: TextOperationTriple -> Bool
prop_compose_length (SOT _ a b) = case compose a b of
  Nothing -> False
  Just ab -> deltaLength a + deltaLength b == deltaLength ab

prop_compose_well_formed :: TextOperationTriple -> Bool
prop_compose_well_formed (SOT _ a b) = case compose a b of
  Nothing -> False
  Just ab -> wellFormed ab

wellFormed :: TextOperation -> Bool
wellFormed (TextOperation ops) = all (not . nullLength) ops
  where nullLength (Retain n) = n == 0
        nullLength (Insert i) = i == ""
        nullLength (Delete d) = d == ""

prop_compose_apply :: TextOperationTriple -> Bool
prop_compose_apply (SOT s a b) = case (apply b `fmap` (apply a s), (\ab -> apply ab s) `fmap`  compose a b) of
  (Just str', Just str'') -> str' == str''
  _ -> False

prop_xform_length :: TextConcurrentOperationTriple -> Bool
prop_xform_length (SCOT _ a b) = case transform a b of
  Nothing -> False
  Just (a', b') -> deltaLength a + deltaLength b' == deltaLength b + deltaLength a'

prop_xform_apply :: TextConcurrentOperationTriple -> Bool
prop_xform_apply (SCOT s a b) = case transform a b of
  Nothing -> False
  Just (a', b') -> case (apply b' `fmap` apply a s, apply a' `fmap` apply b s) of
    (Just s', Just s'') -> s' == s''
    _ -> False

main :: IO ()
main = do
  quickCheck prop_length
  quickCheck prop_compose_length
  quickCheck prop_compose_well_formed
  quickCheck prop_compose_apply
  quickCheck prop_xform_length
  quickCheck prop_xform_apply
