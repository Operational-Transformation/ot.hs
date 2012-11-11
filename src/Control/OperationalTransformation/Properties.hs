module Control.OperationalTransformation.Properties
  ( prop_compose_apply
  , prop_transform_apply
  ) where

import Control.OperationalTransformation
import Test.QuickCheck hiding (Result, reason)
import Test.QuickCheck.Property
import Control.Monad (liftM2, liftM3)

(==?) :: (Eq a, Show a) => a -> a -> Result
a ==? b | a == b    = succeeded
        | otherwise = failed { reason = "expected " ++ show a ++ " to be " ++ show b }

eitherProperty :: (Either String a) -> (a -> Property) -> Property
eitherProperty (Left err) _ = property $ failed { reason = err }
eitherProperty (Right res) prop = prop res

-- | @(b ∘ a)(d) = a(b(d))@ where /a/ and /b/ are two consecutive operations
-- and /d/ is the initial document.
prop_compose_apply :: (OTSystem doc op, OTComposableOperation op, Arbitrary doc, Show doc, Eq doc)
                   => (doc -> Gen op) -> Property
prop_compose_apply genOperation = do
  doc <- arbitrary
  a <- genOperation doc
  eitherProperty (apply a doc) $ \doc' -> do
    b <- genOperation doc'
    eitherProperty (apply b doc') $ \doc'' -> do
      eitherProperty (compose a b) $ \ab -> do
        property $ Right doc'' ==? apply ab doc

-- | @b'(a(d)) = b'(a(d))@ where /a/ and /b/ are random operations, /d/ is the
-- initial document and @(a', b') = transform(a, b)@.
prop_transform_apply :: (OTSystem doc op, Arbitrary doc, Show doc, Eq doc)
                     => (doc -> Gen op)
                     -> Property
prop_transform_apply genOperation = do
  doc <- arbitrary
  a <- genOperation doc
  b <- genOperation doc
  let res1 = liftM3 (,,) (apply a doc) (apply b doc) (transform a b)
  eitherProperty res1 $ \(doca, docb, (a', b')) -> do
    let res2 = liftM2 (,) (apply b' doca) (apply a' docb)
    eitherProperty res2 $ \(docab', docba') ->
      property $ docab' ==? docba'
