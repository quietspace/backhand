{-# LANGUAGE DeriveGeneric, DisambiguateRecordFields, NamedFieldPuns #-}

module Backhand.Channel (
  -- * Clients
  Clients(..),
  addClient,
  delClient,
  Client(..),
  newDefaultClient,
  newClientBoundedBy,

  -- * Services
  Services(..),
  newService,

  -- * Channel
  Channel(..),
  newChannel,

  -- * Channels
  Channels(..),
  newChannels,
  newChannelsIO,
  addChannel,
  addChannelIO,
  addNewChannel,
  delChannel,
  delChannelIO,
  joinChannel,
  joinChannelIO,

  -- * Utility
  BackhandErrorCase(..),
  leaveChannel,
  sendMessage,
  findChannel,
  isChannelPresent,
  broadcast,
  broadcastOthers,
  ) where

import qualified STMContainers.Map as M

import GHC.Generics

import Control.Concurrent.Chan.Unagi.Bounded
import Control.Concurrent.STM
import Control.Concurrent.Unique
import Data.Aeson
import Data.Hashable
import Data.UUID
import ListT
import System.Random (randomIO)

import Backhand.Message
import Backhand.Unagi

-- Clients --

newtype Clients s = Clients { unClients :: M.Map Unique (InChan s) }

addClient :: Client s -> Clients s -> STM ()
addClient Client{ unique, unagi } =
  M.insert (inChan unagi) unique . unClients

delClient :: Client s -> Clients s -> STM ()
delClient Client{ unique } =
  M.delete unique . unClients

data Client s = Client
  { unique :: Unique
  , unagi :: Unagi s
  }

newDefaultClient :: IO (Client s)
newDefaultClient = newClientBoundedBy 10

newClientBoundedBy :: Int -> IO (Client s)
newClientBoundedBy n =
  pure Client <*> newUnique <*> newUnagiBoundedBy n

instance Messenger Client where
  sendWith Client{ unagi } = sendWith unagi
  readWith Client{ unagi } = readWith unagi

-- Modules --

newtype Services t c = Services { unServices :: M.Map t (InChan c) }

newService :: t -> IO (t, Unagi c)
newService t = pure (,) <*> pure t <*> newUnagi

-- Channel --

-- TODO: Rewrite doc
-- | Our product type for channel communications. Requestors are your abstract
-- clients; you will most likely want those clients to be managed and act as the
-- middleman for your providers. Modules are your abstract services which can
-- be anything from a game, a chat server, a client to another server, etc.
data Channel t c s = Channel
  { services :: Services t c
  , clients :: Clients s
  }

-- | Helper function to help you get started.
newChannel :: IO (UUID, Channel t c s)
newChannel = pure (,) <*> randomIO <*> (pure Channel <*> fmap Services M.newIO <*> fmap Clients M.newIO)

-- Channels --

-- | The Channels is the backbone of our routing type. This will be the
-- structure you interact with to start new connections, delete old channels by
-- their `ChanUUID`, and ultimately any action that has to do with creating
-- a new channel.
newtype Channels t c s = Channels { unChannels :: M.Map UUID (Channel t c s) }

-- | Helper function to help you get started.
newChannels :: STM (Channels t c s)
newChannels = fmap Channels M.new

newChannelsIO :: IO (Channels t c s)
newChannelsIO = fmap Channels M.newIO

addChannel :: UUID -> Channel t c s -> Channels t c s -> STM ()
addChannel u channel = M.insert channel u . unChannels

addChannelIO :: UUID -> Channel t c s -> Channels t c s -> IO ()
addChannelIO u ch chm = atomically $ addChannel u ch chm

-- | Convience function for creating a `newChannel` and adding it to an existing
-- `Channels`.
addNewChannel :: Channels t c s -> IO UUID
addNewChannel chm = do
  (u, ch) <- newChannel
  addChannelIO u ch chm
  pure u

delChannel :: UUID -> Channels t c s -> STM ()
delChannel u = M.delete u . unChannels

delChannelIO :: UUID -> Channels t c s -> IO ()
delChannelIO cid chm = atomically $ delChannel cid chm

joinChannel :: Client s -> UUID -> Channels t c s -> STM (Either BackhandErrorCase ())
joinChannel client uuid channels = do
  channel <- M.lookup uuid (unChannels channels)
  case channel of
    Just Channel{ clients } -> do
      addClient client clients
      pure $ Right ()
    Nothing ->
      pure $ Left ChannelNotFound

joinChannelIO :: Client s -> UUID -> Channels t c s -> IO (Either BackhandErrorCase ())
joinChannelIO client uuid channels =
  atomically $ joinChannel client uuid channels

-- Utility --

leaveChannel ::  Channels t c s -> Client s -> UUID -> IO (Either BackhandErrorCase ())
leaveChannel channels client uuid = atomically $ do
  channel <- M.lookup uuid (unChannels channels)
  case channel of
    Just Channel{ clients } -> do
      delClient client clients
      pure $ Right ()
    Nothing ->
      pure $ Left ChannelNotFound

-- | Sending a message from a Client's handler to a Service.
sendMessage :: (Eq t, Hashable t) => Channels t c s -> ConnectionData t c -> IO (Either BackhandErrorCase ())
sendMessage channels ConnectionData{ uuid, service, message } = do
  serviceChan <- atomically $ do
    channel <- M.lookup uuid (unChannels channels)
    case channel of
      Just Channel{ services } ->
        M.lookup service (unServices services)
      Nothing ->
        pure Nothing
  case serviceChan of
    Just chan -> do
      writeChan chan message
      pure $ Right ()
    Nothing ->
      pure $ Left SendFailed

findChannel :: Channels t c s -> UUID -> IO (Maybe (UUID, [t]))
findChannel channels uuid =
  atomically $ do
    channel <- M.lookup uuid (unChannels channels)
    case channel of
      Just Channel{ services } -> do
        servicel <- fold (\ r (mText, _) -> pure (mText : r) ) [] $ M.stream (unServices services)
        pure $ Just (uuid, servicel)
      Nothing -> pure Nothing

-- | If channel exists by our `UUID`
isChannelPresent :: Channels t c s -> UUID -> IO Bool
isChannelPresent channels uuid = do
  channel <- atomically $ M.lookup uuid (unChannels channels)
  pure $ case channel of
    Just _ -> True
    Nothing -> False

broadcast :: Clients s -> s -> IO ()
broadcast clients reply = do
  channels <- atomically $
    fold (\ r (_, inChan) -> pure (inChan : r) ) [] $ M.stream (unClients clients)
  mapM_ (`writeChan` reply) channels

broadcastOthers :: Unique -> Clients s -> s -> IO ()
broadcastOthers u clients reply = do
  channels <- atomically $
    fold (\ r (u', inChan) ->
            if u' /= u
            then pure (inChan : r)
            else pure r
         ) [] $ M.stream (unClients clients)
  mapM_ (`writeChan` reply) channels

-- | Likely you'll want to know if something goes wrong and then either report
-- or ignore the outcome. Handling these manually might be a little cumbersome,
-- but if you pass it to a system that takes errors and returns them to a person
-- of interest or log them somewhere then it might be more useful.
data BackhandErrorCase
  = ChannelNotFound
  | SendFailed
  deriving Generic

instance ToJSON BackhandErrorCase
