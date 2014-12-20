{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

module Control.OperationalTransformation.Selection.Tests (tests) where

import Control.OperationalTransformation
import Control.OperationalTransformation.Text
import Control.OperationalTransformation.Selection
import Test.Framework
import Test.QuickCheck
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit hiding (Test)
import Test.Framework.Providers.HUnit (testCase)
import Control.Applicative
--import Data.Aeson
import Data.Aeson.Types hiding (Result)
import Data.Monoid

instance Arbitrary Range where
  arbitrary = Range <$> (abs <$> arbitrary) <*> (abs <$> arbitrary)

deriving instance Arbitrary Selection

prop_json_id :: Selection -> Bool
prop_json_id o = parseMaybe parseJSON (toJSON o) == Just o

testUpdateRange :: Assertion
testUpdateRange = do
  let range = Range 3 7
  Range 8 10 @=? updateCursor (TextOperation [Retain 3, Insert "lorem", Delete 2, Retain 42]) range
  Range 0 0  @=? updateCursor (TextOperation [Delete 45]) range

testUpdateSelection :: Assertion
testUpdateSelection =
  let sel = [Range 2 4, Range 6 8] :: Selection
  in [Range 2 6, Range 8 8] @=? updateCursor (TextOperation [Retain 3, Insert "hi", Retain 3, Delete 2]) sel

{-
testUpdateCursor :: Assertion
testUpdateCursor = do

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
    b  = AugmentedTextOperation (Cursor 0 1)   (TextOperation [Insert "L", Delete 1, Retain 10])
    ab = AugmentedTextOperation (Cursor 0 1)   (TextOperation [Delete 1, Insert "L", Retain 4, Insert " Ipsum"])
    Right ab' = a `compose` b

testApplyAugmented :: Assertion
testApplyAugmented = apply op ("lorem" :: T.Text) @=? Right "lorem ipsum"
  where op = AugmentedTextOperation (Cursor 0 2) (TextOperation [Retain 5, Insert " ipsum"])
-}

testSize :: Assertion
testSize = do
  size (createCursor 5) @=? 0
  size [Range 5 8, Range 10 20] @=? 13

prop_sizeNotNegative :: Selection -> Bool
prop_sizeNotNegative sel = size sel >= 0

prop_sizeAdditive :: Selection -> Selection -> Bool
prop_sizeAdditive sel1 sel2 =
  size sel1 + size sel2 == size (sel1 <> sel2)

prop_sizeZero_notSomethingSelected :: Selection -> Bool
prop_sizeZero_notSomethingSelected sel =
  (size sel /= 0) == (somethingSelected sel)

tests :: Test
tests = testGroup "Control.OperationalTransformation.Selection"
  [ testProperty "prop_json_id" prop_json_id
  , testCase "testUpdateRange" testUpdateRange
  , testCase "testUpdateSelection" testUpdateSelection
  , testCase "testSize" testSize
  , testProperty "prop_sizeNotNegative" prop_sizeNotNegative
  , testProperty "prop_sizeAdditive" prop_sizeAdditive
  , testProperty "prop_sizeZero_notSomethingSelected" prop_sizeZero_notSomethingSelected
  ]