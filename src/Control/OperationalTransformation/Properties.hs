module Control.OperationalTransformation.Properties
  ( prop_compose_apply
  , prop_transform_apply
  ) where

import Control.OperationalTransformation
import Test.QuickCheck hiding (Result, reason)
import Test.QuickCheck.Property
import Control.Monad (join)

(==?) :: (Eq a, Show a) => a -> a -> Result
a ==? b | a == b    = succeeded
        | otherwise = failed { reason = "expected " ++ show a ++ " to be " ++ show b }

prop_compose_apply :: (OTSystem doc op, OTComposableOperation op, Arbitrary doc, Show doc, Eq doc)
                   => (doc -> Gen op) -> Property
prop_compose_apply genOperation = join . fmap property $ do
  doc <- arbitrary
  a <- genOperation doc
  case apply a doc of
    Nothing -> return rejected
    Just doc' -> do
      b <- genOperation doc'
      case (apply b doc', compose a b) of
        (Nothing, _) -> return rejected
        (Just _, Nothing) -> return $ failed { reason = "compose resulted in Nothing" }
        (Just doc'', Just ab) -> do
          return $ Just doc'' ==? apply ab doc

prop_transform_apply :: (OTSystem doc op, Arbitrary doc, Show doc, Eq doc)
                     => (doc -> Gen op) -> Property
prop_transform_apply genOperation = join . fmap property $ do
  doc <- arbitrary
  a <- genOperation doc
  b <- genOperation doc
  case (apply a doc, apply b doc, transform a b) of
    (Nothing, _, _) -> return rejected
    (_, Nothing, _) -> return rejected
    (_, _, Nothing) -> return $ failed { reason = "transform failed" }
    (Just doca, Just docb, Just (a', b')) ->
      case (apply b' doca, apply a' docb) of
        (Just docab', Just docba') -> return $ docab' ==? docba'
        _ -> return $ failed { reason = "couldn't apply transformed operation" }