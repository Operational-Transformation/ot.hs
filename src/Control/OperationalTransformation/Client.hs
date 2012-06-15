module Control.OperationalTransformation.Client
  ( ClientState (..)
  , ClientAction (..)
  , initialClientState
  , applyClient
  , applyServer
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
            => ClientState op -> op -> (ClientAction op, ClientState op)
applyClient ClientSynchronized op = (SendOperation op, ClientWaiting op)
applyClient (ClientWaiting w) op = (NoAction, ClientWaitingWithBuffer w op)
applyClient (ClientWaitingWithBuffer w b) op = case compose b op of
  Nothing -> error "operations couldn't be composed"
  Just b' -> (NoAction, ClientWaitingWithBuffer w b')

applyServer :: (OTComposableOperation op)
            => ClientState op -> Bool -> op -> (ClientAction op, ClientState op)
applyServer ClientSynchronized False op = (ApplyOperation op, ClientSynchronized)
applyServer ClientSynchronized True _ = error $ "got an operation from server that's supposedly an operation from this client" ++
                                                "altough according to the state the client isn't waiting for an outstanding operation"
applyServer (ClientWaiting w) False op = case transform w op of
  Nothing -> error "transform failed a"
  Just (w', op') -> (ApplyOperation op', ClientWaiting w')
applyServer (ClientWaiting _) True _ = (NoAction, ClientSynchronized)
applyServer (ClientWaitingWithBuffer w b) False op = case transform w op of
  Nothing -> error "transform failed b"
  Just (w', op') -> case transform b op' of
    Nothing -> error "transform failed c"
    Just (b', op'') -> (ApplyOperation op'', ClientWaitingWithBuffer w' b')
applyServer (ClientWaitingWithBuffer _ b) True _ = (SendOperation b, ClientWaiting b)