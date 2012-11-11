module Control.OperationalTransformation.Server
  ( Revision
  , ServerState (..)
  , initialServerState
  , applyOperation
  ) where

import Control.OperationalTransformation
import Control.Monad (foldM)

type Revision = Integer

-- | The server keeps the current revision number and a list of previous
-- operations to transform incoming operations against.
data ServerState doc op = ServerState Revision doc [op]

initialServerState :: doc -> ServerState doc op
initialServerState doc = ServerState 0 doc []

-- | Handles incoming operations.
applyOperation :: (OTSystem doc op)
               => ServerState doc op
               -> Revision
               -- ^ The latest operation that the client has received from the server when it sent the operation.
               -> op
               -- ^ The operation received from the client.
               -> Either String (op, ServerState doc op)
               -- ^ The operation to broadcast to all connected clients
               -- (except the client which has created the operation; that
               -- client must be sent an acknowledgement) and the new state
               -- (or an error).
applyOperation (ServerState rev doc ops) oprev op = do
  concurrentOps <- if oprev > rev || rev - oprev > fromIntegral (length ops)
    then Left "unknown revision number"
    else Right $ take (fromInteger $ rev - oprev) ops
  op' <- foldM transformFst op (reverse concurrentOps)
  doc' <- case apply op' doc of
    Left err -> Left $ "apply failed: " ++ err
    Right d -> Right d
  return $ (op', ServerState (rev+1) doc' (op':ops))
  where
    transformFst a b = case transform a b of
      Left err -> Left $ "transform failed: " ++ err
      Right (a', _) -> Right a'
