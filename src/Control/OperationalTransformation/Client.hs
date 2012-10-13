module Control.OperationalTransformation.Client
  ( ClientState (..)
  , ClientAction (..)
  , initialClientState
  , applyClient
  , applyServer
  , serverAck
  ) where

import Control.OperationalTransformation

data ClientState op = ClientSynchronized
                    | ClientWaiting op
                    | ClientWaitingWithBuffer op op
                    deriving (Eq, Show, Read)

data ClientAction op = NoAction
                     | ApplyOperation op
                     | SendOperation op
                     deriving (Eq, Show, Read)

initialClientState :: ClientState op
initialClientState = ClientSynchronized

applyClient :: (OTComposableOperation op)
            => ClientState op -> op -> Either String (ClientAction op, ClientState op)
applyClient ClientSynchronized op = Right (SendOperation op, ClientWaiting op)
applyClient (ClientWaiting w) op = Right (NoAction, ClientWaitingWithBuffer w op)
applyClient (ClientWaitingWithBuffer w b) op = case compose b op of
  Left err -> Left $ "operations couldn't be composed: " ++ err
  Right b' -> Right (NoAction, ClientWaitingWithBuffer w b')

applyServer :: (OTComposableOperation op)
            => ClientState op -> op -> Either String (ClientAction op, ClientState op)
applyServer ClientSynchronized op = Right (ApplyOperation op, ClientSynchronized)
applyServer (ClientWaiting w) op = case transform w op of
  Left err -> Left $ "transform failed: " ++ err
  Right (w', op') -> Right (ApplyOperation op', ClientWaiting w')
applyServer (ClientWaitingWithBuffer w b) op = case transform w op of
  Left err -> Left $ "transform failed: " ++ err
  Right (w', op') -> case transform b op' of
    Left err -> Left $ "transform failed: " ++ err
    Right (b', op'') -> Right (ApplyOperation op'', ClientWaitingWithBuffer w' b')

serverAck :: ClientState op -> Maybe (ClientAction op, ClientState op)
serverAck ClientSynchronized            = Nothing
serverAck (ClientWaiting _)             = Just (NoAction, ClientSynchronized)
serverAck (ClientWaitingWithBuffer _ b) = Just (SendOperation b, ClientWaiting b)