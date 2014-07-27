module Control.OperationalTransformation.Properties
  ( prop_compose_apply
  , prop_transform_apply
  , prop_transform_compose
  , prop_transform_compose_compat_l
  , prop_transform_compose_compat_r
  ) where

import Control.OperationalTransformation
import Test.QuickCheck hiding (Result, reason)
import Test.QuickCheck.Property
import Control.Applicative ((<$>), (<*>))

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
prop_compose_apply genOperation = property $ do
  doc <- arbitrary
  a <- genOperation doc
  return $ eitherProperty (apply a doc) $ \doc' -> property $ do
    b <- genOperation doc'
    return $ eitherProperty ((,) <$> apply b doc' <*> compose a b) $ \(doc'', ab) ->
      property $ Right doc'' ==? apply ab doc

-- | @b'(a(d)) = a'(b(d))@ where /a/ and /b/ are random operations, /d/ is the
-- initial document and @(a', b') = transform(a, b)@.
prop_transform_apply :: (OTSystem doc op, Arbitrary doc, Show doc, Eq doc)
                     => (doc -> Gen op)
                     -> Property
prop_transform_apply genOperation = property $ do
  doc <- arbitrary
  a <- genOperation doc
  b <- genOperation doc
  let res1 = (,,) <$> apply a doc <*> apply b doc <*> transform a b
  return $ eitherProperty res1 $ \(doca, docb, (a', b')) ->
    let res2 = (,) <$> apply b' doca <*> apply a' docb
    in eitherProperty res2 $ \(docab', docba') ->
      property $ docab' ==? docba'

-- | @b' ∘ a = a' ∘ b@ where /a/ and /b/ are random operations and
-- @(a', b') = transform(a, b)@. Note that this is a stronger property than
-- prop_transform_apply, because prop_transform_compose and
-- prop_compose_apply imply prop_transform_apply.
prop_transform_compose :: (OTSystem doc op, OTComposableOperation op, Arbitrary doc, Show op, Eq op)
                      => (doc -> Gen op)
                      -> Property
prop_transform_compose genOperation = property $ do
  doc <- arbitrary
  a <- genOperation doc
  b <- genOperation doc
  return $ eitherProperty (transform a b) $ \(a', b') ->
    eitherProperty ((,) <$> compose a b' <*> compose b a') $ \(ab', ba') ->
      property $ ab' ==? ba'

-- | Transformation is compatible with composition on the left. That is, if we
-- have two consecutive operations /a/ and /b/ and a concurrent operation /c/,
-- then it doesn't make a difference whether we transform /c/ against /a/ and
-- then against /b/ or transform /c/ against the composition of /a/ and /b/.
-- In other terms, @c'_1 = c'_2@ where @(_, c'_1) = transform(b ∘ a, c)@,
-- @(_, c') = transform(a, c)@ and @(_, c'_2) = transform(b, c')@.
prop_transform_compose_compat_l :: (OTSystem doc op, OTComposableOperation op, Arbitrary doc, Show op, Eq op)
                                => (doc -> Gen op)
                                -> Property
prop_transform_compose_compat_l genOperation = property $ do
  doc <- arbitrary
  a <- genOperation doc
  c <- genOperation doc
  return $ eitherProperty (apply a doc) $ \(doc') -> property $ do
    b <- genOperation doc'
    let res = (,) <$> (snd <$> (compose a b >>= flip transform c))
                  <*> (snd <$> (transform a c >>= transform b . snd))
    return $ eitherProperty res $ \(c'_1, c'_2) ->
      property $ c'_1 ==? c'_2

-- | Transformation is compatible with composition on the /right/.
prop_transform_compose_compat_r :: (OTSystem doc op, OTComposableOperation op, Arbitrary doc, Show op, Eq op)
                                => (doc -> Gen op)
                                -> Property
prop_transform_compose_compat_r genOperation = property $ do
  doc <- arbitrary
  a <- genOperation doc
  c <- genOperation doc
  return $ eitherProperty (apply a doc) $ \(doc') -> property $ do
    b <- genOperation doc'
    let res = (,) <$> (fst <$> (compose a b >>= transform c))
                  <*> (fst <$> (transform c a >>= flip transform b . fst))
    return $ eitherProperty res $ \(c'_1, c'_2) -> property $ c'_1 ==? c'_2
