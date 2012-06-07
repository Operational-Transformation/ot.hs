{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}

module Control.OperationalTransformation
  ( OTOperation (..)
  , OTComposableOperation (..)
  , OTSystem (..)
  ) where

class OTOperation op where
  transform :: op -> op -> Maybe (op, op)

class (OTOperation op) => OTComposableOperation op where
  compose :: op -> op -> Maybe op

class (OTOperation op) => OTSystem doc op where
  apply :: op -> doc -> Maybe doc

data OpList op = SingleOp op
               | ConsecutiveOps op (OpList op)
               deriving (Show, Read, Eq)

instance (OTOperation op) => OTOperation (OpList op) where
  transform = transformList2
    where
      transformList1 o (SingleOp p) = do
        (o', p') <- transform o p
        return (o', SingleOp p')
      transformList1 o (ConsecutiveOps p ps) = do
        (o', p') <- transform o p
        (o'', ps') <- transformList1 o' ps
        return (o'', ConsecutiveOps p' ps')

      transformList2 (SingleOp o) ps = do
        (o', ps') <- transformList1 o ps
        return (SingleOp o', ps')
      transformList2 (ConsecutiveOps o os) ps = do
        (o', ps') <- transformList1 o ps
        (os', ps'') <- transformList2 os ps'
        return (ConsecutiveOps o' os', ps'')

instance (OTOperation op) => OTComposableOperation (OpList op) where
  compose a b = Just $ loop a b
    where
      loop (SingleOp op) ops = ConsecutiveOps op ops
      loop (ConsecutiveOps op ops) ops2 = ConsecutiveOps op $ loop ops ops2

instance (OTSystem doc op) => OTSystem doc (OpList op) where
  apply (SingleOp op) doc = apply op doc
  apply (ConsecutiveOps op ops) doc = apply op doc >>= apply ops
