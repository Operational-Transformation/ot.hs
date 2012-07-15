module Control.OperationalTransformation.Server
  ( Revision
  , ServerState (..)
  , initialServerState
  , applyOperation
  ) where

import Control.OperationalTransformation
import Control.Monad (foldM)

type Revision = Integer
data ServerState doc op = ServerState Revision doc [op]

initialServerState :: doc -> ServerState doc op
initialServerState doc = ServerState 0 doc []

applyOperation :: (OTSystem doc op)
               => ServerState doc op -> Revision -> op -> Either String (op, ServerState doc op)
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
