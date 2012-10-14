module Control.OperationalTransformation.Client
  ( ClientState (..)
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

initialClientState :: ClientState op
initialClientState = ClientSynchronized

applyClient :: (OTComposableOperation op)
            => ClientState op
            -> op
            -> Either String (Bool, ClientState op)
applyClient ClientSynchronized op = Right (True, ClientWaiting op)
applyClient (ClientWaiting w) op = Right (False, ClientWaitingWithBuffer w op)
applyClient (ClientWaitingWithBuffer w b) op = case compose b op of
  Left err -> Left $ "operations couldn't be composed: " ++ err
  Right b' -> Right (False, ClientWaitingWithBuffer w b')

applyServer :: (OTComposableOperation op)
            => ClientState op
            -> op
            -> Either String (op, ClientState op)
applyServer ClientSynchronized op = Right (op, ClientSynchronized)
applyServer (ClientWaiting w) op = case transform w op of
  Left err -> Left $ "transform failed: " ++ err
  Right (w', op') -> Right (op', ClientWaiting w')
applyServer (ClientWaitingWithBuffer w b) op = case transform w op of
  Left err -> Left $ "transform failed: " ++ err
  Right (w', op') -> case transform b op' of
    Left err -> Left $ "transform failed: " ++ err
    Right (b', op'') -> Right (op'', ClientWaitingWithBuffer w' b')

serverAck :: ClientState op -> Maybe (Maybe op, ClientState op)
serverAck ClientSynchronized            = Nothing
serverAck (ClientWaiting _)             = Just (Nothing, ClientSynchronized)
serverAck (ClientWaitingWithBuffer _ b) = Just (Just b, ClientWaiting b)