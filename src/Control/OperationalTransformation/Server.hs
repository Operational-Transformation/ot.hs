module Control.OperationalTransformation.Server
  ( Revision
  , ServerState (..)
  , initialServerState
  , applyOperation
  ) where

import Control.OperationalTransformation

type Revision = Integer
data ServerState doc op = ServerState Revision doc [op]

initialServerState :: doc -> ServerState doc op
initialServerState doc = ServerState 0 doc []

applyOperation :: (OTSystem doc op)
               => ServerState doc op -> Revision -> op -> (op, ServerState doc op)
applyOperation (ServerState rev doc ops) oprev op = (op', ServerState (rev+1) doc' (op':ops))
  where
    concurrentOps = if oprev > rev || rev - oprev > fromIntegral (length ops)
      then error "unknown revision number"
      else take (fromInteger $ rev - oprev) ops
    transformFst a b = case transform a b of
      Left err -> error $ "transform failed: " ++ err
      Right (a', _) -> a'
    op' = foldr (flip transformFst) op concurrentOps
    doc' = case apply op' doc of
      Left err -> error $ "apply failed: " ++ err
      Right d -> d
