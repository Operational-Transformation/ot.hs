{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

module Control.OperationalTransformation.Text.Tests
  ( tests
  , genOperation
  ) where

import Control.OperationalTransformation
import Control.OperationalTransformation.Text
import Control.OperationalTransformation.Properties

import Test.QuickCheck hiding (Result)
import Test.QuickCheck.Property
import Test.HUnit hiding (Test)
import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)

import qualified Data.Text as T
import Data.String (fromString)
import Data.Binary (encode, decode)
import Control.Monad (join, liftM, liftM2)
import Control.Applicative ((<$>), (<*>))
import Data.Aeson.Types hiding (Result)

instance Arbitrary T.Text where
  arbitrary = liftM fromString arbitrary

instance Arbitrary TextOperation where
  arbitrary = arbitrary >>= genOperation

instance Arbitrary Cursor where
  arbitrary = Cursor <$> (abs <$> arbitrary) <*> (abs <$> arbitrary)

instance Arbitrary AugmentedTextOperation where
  arbitrary = AugmentedTextOperation <$> arbitrary <*> arbitrary

genOperation :: T.Text -> Gen TextOperation
genOperation = liftM TextOperation . gen
  where
    gen "" = oneof [return [], liftM ((:[]) . Insert) (arbitraryText maxLength)]
    gen s = do
      len <- choose (1, min maxLength (T.length s))
      oneof [ liftM (Retain len :) $ gen (T.drop len s)
            , do s2 <- arbitraryText len
                 liftM (Insert s2 :) $ gen s
            , liftM (Delete len :) $ gen (T.drop len s)
            ]
    maxLength = 32
    arbitraryText n = liftM (fromString . take n) $ listOf1 arbitrary

deltaLength :: TextOperation -> Int
deltaLength (TextOperation ops) = sum (map len ops)
  where len (Retain _)   = 0
        len (Insert i) = T.length i
        len (Delete d) = -d

prop_json_id :: AugmentedTextOperation -> Bool
prop_json_id o = parseMaybe parseJSON (toJSON o) == Just o

prop_binary_id :: TextOperation -> Bool
prop_binary_id o = decode (encode o) == o

prop_apply_length :: T.Text -> Property
prop_apply_length doc = join $ do
  op <- genOperation doc
  return . property $ case apply op doc of
    Left _ -> False
    Right str' -> T.length str' == T.length doc + deltaLength op

prop_compose_length :: T.Text -> Property
prop_compose_length doc = join $ do
  a <- genOperation doc
  case apply a doc of
    Left _ -> return $ property rejected
    Right doc' -> do
      b <- genOperation doc'
      return . property $ case compose a b of
        Left _ -> False
        Right ab -> deltaLength a + deltaLength b == deltaLength ab

prop_compose_well_formed :: T.Text -> Property
prop_compose_well_formed doc = join $ fmap property $ do
  a <- genOperation doc
  let Right doc' = apply a doc
  b <- genOperation doc'
  return $ case compose a b of
    Left _ -> False
    Right ab -> wellFormed ab

prop_transform_well_formed :: T.Text -> Property
prop_transform_well_formed doc = join $ fmap property $ do
  a <- genOperation doc
  b <- genOperation doc
  return $ case transform a b of
    Left _ -> False
    Right (a', b') -> wellFormed a' && wellFormed b'

wellFormed :: TextOperation -> Bool
wellFormed (TextOperation ops) = all (not . nullLength) ops
  where nullLength (Retain n) = n == 0
        nullLength (Insert i) = i == ""
        nullLength (Delete d) = d == 0

prop_invert :: T.Text -> Gen Bool
prop_invert doc = do
  op <- genOperation doc
  return $ case liftM2 (,) (invertOperation op doc) (apply op doc) of
    Left _ -> False
    Right (invOp, doc') -> case apply invOp doc' of
      Left _ -> False
      Right doc2 -> doc2 == doc

testUpdateCursor :: Assertion
testUpdateCursor = do
  let cursor = Cursor 3 7
  Cursor 8 10 @=? updateCursor cursor (TextOperation [Retain 3, Insert "lorem", Delete 2, Retain 42])
  Cursor 0 0  @=? updateCursor cursor (TextOperation [Delete 45])

testTransformAugmented :: Assertion
testTransformAugmented = (a' @=? a'') >> (b' @=? b'')
  where
    a  = AugmentedTextOperation (Cursor 5  5)  (TextOperation [Delete 5, Retain 10])
    b  = AugmentedTextOperation (Cursor 10 15) (TextOperation [Retain 10, Insert "lorem", Retain 5])
    a' = AugmentedTextOperation (Cursor 10 10) (TextOperation [Delete 5, Retain 15])
    b' = AugmentedTextOperation (Cursor 5  10) (TextOperation [Retain 5, Insert "lorem", Retain 5])
    Right (a'', b'') = a `transform` b

testComposeAugmented :: Assertion
testComposeAugmented = ab @=? ab'
  where
    a  = AugmentedTextOperation (Cursor 11 11) (TextOperation [Retain 5, Insert " Ipsum"])
    b  = AugmentedTextOperation (Cursor 0 1)   (TextOperation [Delete 1, Insert "L", Retain 10])
    ab = AugmentedTextOperation (Cursor 0 1)   (TextOperation [Delete 1, Insert "L", Retain 4, Insert " Ipsum"])
    Right ab' = a `compose` b

testApplyAugmented :: Assertion
testApplyAugmented = apply op ("lorem" :: T.Text) @=? Right "lorem ipsum"
  where op = AugmentedTextOperation (Cursor 0 2) (TextOperation [Retain 5, Insert " ipsum"])

tests :: Test
tests = testGroup "Control.OperationalTransformation.Text.Tests"
  [ testProperty "prop_json_id" prop_json_id
  , testProperty "prop_binary_id" prop_binary_id
  , testProperty "prop_compose_apply" $ prop_compose_apply genOperation
  , testProperty "prop_transform_apply" $ prop_transform_apply genOperation
  , testProperty "prop_apply_length" prop_apply_length
  , testProperty "prop_compose_length" prop_compose_length
  , testProperty "prop_compose_well_formed" prop_compose_well_formed
  , testProperty "prop_transform_well_formed" prop_transform_well_formed
  , testProperty "prop_invert" prop_invert
  , testCase "testUpdateCursor" testUpdateCursor
  , testCase "testTransformAugmented" testTransformAugmented
  , testCase "testComposeAugmented" testComposeAugmented
  , testCase "testApplyAugmented" testApplyAugmented
  ]
