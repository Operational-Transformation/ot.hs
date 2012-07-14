{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, BangPatterns #-}

module Control.OperationalTransformation.ClientServerTests
  ( tests
  ) where

import Control.OperationalTransformation
import Control.OperationalTransformation.Client
import Control.OperationalTransformation.Server
import Data.Maybe (fromJust)
import Control.Monad (join)

import Control.OperationalTransformation.Text.Tests (genOperation)

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck hiding (reason)
import Test.QuickCheck.Property

type Queue a = [a]

emptyQueue :: Queue a
emptyQueue = []

appendQueue :: a -> Queue a -> Queue a
appendQueue a q = q ++ [a]

type ClientId = Int

data ExtendedOperation op = ExtendedOperation !ClientId !Revision !op

instance (OTOperation op) => OTOperation (ExtendedOperation op) where
  transform (ExtendedOperation cida reva opa) (ExtendedOperation cidb revb opb)
    | cida == cidb = Nothing
    | otherwise    = case transform opa opb of
        Nothing -> Nothing
        Just (a', b') -> Just (ExtendedOperation cida (reva+1) a', ExtendedOperation cidb (revb+1) b')

instance (OTComposableOperation op) => OTComposableOperation (ExtendedOperation op) where
  compose (ExtendedOperation cid rev a) (ExtendedOperation _ _ b) = case compose a b of
    Nothing -> Nothing
    Just ab -> Just $ ExtendedOperation cid rev ab

instance (OTSystem doc op) => OTSystem doc (ExtendedOperation op) where
  apply (ExtendedOperation _ _ op) doc = apply op doc

data ExtendedClient doc op = ExtendedClient { clientId :: !ClientId
                                            , clientRevision :: !Revision
                                            , clientSendQueue :: Queue op
                                            , clientReceiveQueue :: Queue op
                                            , clientDoc :: !doc
                                            , clientState :: !(ClientState op)
                                            } deriving (Show)

nextRevision :: ExtendedClient doc op -> Revision
nextRevision client = clientRevision client + case clientState client of
  ClientSynchronized -> 0
  ClientWaiting _ -> 1
  ClientWaitingWithBuffer _ _ -> 2

prop_client_server :: (Eq doc, Arbitrary doc, OTSystem doc op, OTComposableOperation op)
                   => (doc -> Gen op) -> Property
prop_client_server genOp = join $ do
  doc <- arbitrary
  let server = initialServerState doc
      clients = createClients doc $ take numClients [1..]
  (server', clients') <- simulate numActions server clients
  return $ if not (all isSynchronized clients')
    then property $ failed { reason = "some clients are not synchronized" }
    else let ServerState _ doc' _ = server'
         in if all ((== doc') . clientDoc) clients'
              then property True
              else property $ failed { reason = "client documents did not converge" }

  where
    numClients, numActions :: Int
    numClients = 2
    numActions = 100

    firstRevision = 0
    createClients doc = map $ \n ->
      ExtendedClient { clientId = n
                     , clientRevision = firstRevision
                     , clientSendQueue = emptyQueue
                     , clientReceiveQueue = emptyQueue
                     , clientDoc = doc
                     , clientState = initialClientState
                     }

    simulate !n !server !clients = do
      clientN <- choose (0, length clients - 1)
      actionN <- choose (0, 2) :: Gen Int
      let client = clients !! clientN
      (server', clients') <- case actionN of
        0 | canReceive client -> do
          let client' = receiveClient client
          return (server, replace clientN client' clients)
        1 | canSend client -> do
          let (op, client') = sendClient client
              (op', server') = receiveServer server op
              clients' = replace clientN client' clients
              clients'' = broadcast op' clients'
          return (server', clients'')
        _ | n < 0 -> return (server, clients)
          | otherwise -> do
          client' <- editClient client
          return (server, replace clientN  client' clients)
      if n > 0 || any (\c -> canReceive c || canSend c) clients'
        then simulate (n-1) server' clients'
        else return (server', clients')

    replace 0 e (_:xs) = e:xs
    replace n e (x:xs) = x:(replace (n-1) e xs)
    replace _ _ [] = error "replacing empty list"

    canReceive = not . null . clientReceiveQueue
    canSend = not . null . clientSendQueue

    receiveClient client = case clientReceiveQueue client of
      [] -> error "empty receive queue"
      eo@(ExtendedOperation cid _ _):ops ->
        let (action, state') = applyServer (clientState client) (cid == clientId client) eo
            client' = client { clientReceiveQueue = ops
                             , clientState = state'
                             , clientRevision = clientRevision client + 1
                             }
        in case action of
          NoAction -> client'
          ApplyOperation op -> case apply op (clientDoc client') of
            Nothing -> error "apply failed"
            Just doc' -> client' { clientDoc = doc' }
          SendOperation op -> client' { clientSendQueue = appendQueue op (clientSendQueue client') }

    sendClient client = case clientSendQueue client of
      [] -> error "empty send queue"
      op:ops -> (op, client { clientSendQueue = ops })

    editClient client = do
      op <- genOp $ clientDoc client
      let eop = ExtendedOperation (clientId client) (nextRevision client) op
          doc' = fromJust $ apply eop $ clientDoc client
          (action, state') = applyClient (clientState client) eop
          client' = client { clientState = state', clientDoc = doc' }
      return $ case action of
        ApplyOperation _ -> error "shouldn't happen"
        NoAction -> client'
        SendOperation eop' -> client' { clientSendQueue = appendQueue eop' (clientSendQueue client) }

    receiveServer server eo@(ExtendedOperation _ rev _) = applyOperation server rev eo

    broadcast op = map $ \client -> client { clientReceiveQueue = appendQueue op (clientReceiveQueue client) }

    isSynchronized client = case clientState client of
      ClientSynchronized -> True
      _ -> False

tests :: Test
tests = testGroup "Control.OperationalTransformation.ClientServerTests"
  [ testProperty "prop_client_server" $ prop_client_server genOperation
  ]