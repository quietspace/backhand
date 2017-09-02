{-# LANGUAGE DeriveGeneric #-}

module Backhand.Channel where

import qualified STMContainers.Map as M

import GHC.Generics

import Control.Concurrent.Chan.Unagi.Bounded
import Control.Concurrent.STM
import Data.Aeson
import Data.Semigroup.Bitraversable
import Data.Text
import ListT

import Backhand.Message
import Backhand.Unique

-- * Types

type Unagi a = (InChan a, OutChan a)

-- ** Requesters

type Requesters c = M.Map UniqueRequester (InChan c)

newRequester :: IO (UniqueRequester, Unagi c)
newRequester = pure (,) <*> newRequesterId <*> newChan 10

-- ** Modules

type Modules c = M.Map Text (InChan c)

newModule :: Text -> IO (Text, Unagi c)
newModule t = pure (,) <*> pure t <*> newChan 10

-- ** Channel

-- | Our product type for channel communications. Requestors are your abstract
-- clients; you will most likely want those clients to be managed and act as the
-- middleman for your providers. Modules are your abstract services which can
-- be anything from a game, a chat server, a client to another server, etc.
type Channel clientMsg serverMsg = (Requesters serverMsg, Modules clientMsg)

-- | Helper function to help you get started.
newChannel :: IO (ChanUUID, Channel c s)
newChannel = bitraverse1 id bisequence1 (newChanUUID, (M.newIO, M.newIO))

-- ** ChannelMap

-- | The ChannelMap is the backbone of our routing type. This will be the
-- structure you interact with to start new connections, delete old channels by
-- their `ChanUUID`, and ultimately any action that has to do with creating
-- a new channel.
type ChannelMap clientMsg serverMsg = M.Map ChanUUID (Channel clientMsg serverMsg)

-- | Helper function to help you get started.
newChannelMap :: STM (ChannelMap c s)
newChannelMap = M.new

-- * Utility

-- | Interacting with `Channel`s and `ChannelMap`s are the reason this library
-- exists so we provide some very basic functions for interactions between them.
-- You will want to wrap these in your own logic; such as creating a new
-- chatroom you'll add that chatroom instance (in this case a `Channel`) to the
-- chat server (in this case the `ChannelMap`).

addChannel :: ChanUUID -> Channel c s -> ChannelMap c s -> STM ()
addChannel u (r, m) = M.insert (r, m) u

addChannel' :: ChanUUID -> Channel c s -> ChannelMap c s -> IO ()
addChannel' u ch chm = atomically $ addChannel u ch chm

addNewChannel :: ChannelMap c s -> IO ChanUUID
addNewChannel chm = do
    (u, ch) <- newChannel
    addChannel' u ch chm
    pure u

delChannel :: ChanUUID -> ChannelMap c s -> STM ()
delChannel = M.delete

delChannel' :: ChanUUID -> ChannelMap c s -> IO ()
delChannel' cid chm = atomically $ delChannel cid chm

joinChannel :: UniqueRequester -> InChan s -> ChanUUID -> ChannelMap c s -> STM BackhandChannelStatus
joinChannel rid inChan uuid cmap = do
  channel <- M.lookup uuid cmap
  case channel of
    Just (r, _) -> do
      M.insert inChan rid r
      pure ChannelJoinSuccess
    Nothing -> pure NoChannelFound

joinChannel' :: UniqueRequester -> InChan s -> ChanUUID -> ChannelMap c s -> IO BackhandChannelStatus
joinChannel' rid inChan uuid cmap =
  atomically $ joinChannel rid inChan uuid cmap

leaveChannel ::  ChannelMap c s -> UniqueRequester -> ChanUUID -> IO ()
leaveChannel cmap rid chanUUID = atomically $ do
  channel <- M.lookup chanUUID cmap
  case channel of
    Just (r, _) ->
      M.delete rid r
    Nothing ->
      pure ()

sendMessage :: ChannelMap c s -> ConnectionData c -> IO BackhandMessageStatus
sendMessage cmap (ConnectionData chanUUID moduleUUID clientMessage) = do
  modChan <- atomically $ do
    channel <- M.lookup chanUUID cmap
    case channel of
      Just (_, m) ->
        M.lookup moduleUUID m
      Nothing ->
        pure Nothing
  case modChan of
    Just chan -> do
      writeChan chan clientMessage
      pure SendSuccess
    Nothing ->
      pure SendFailure

-- NOTE: Introduce Text tags for modules would reduce dev usage for describing channels.
-- findChannel :: ChannelMap c s -> ChanUUID -> (ChanUUID, [(Text, ModuleUUID)])

-- | If channel exists by our `ChanUUID`
isChannelPresent :: ChannelMap c s -> ChanUUID -> IO Bool
isChannelPresent cmap chanUUID = do
  channel <- atomically $ M.lookup chanUUID cmap
  pure $ case channel of
    Just _ -> True
    Nothing -> False

broadcast :: Requesters s -> s -> IO ()
broadcast rmap reply = do
  channels <- atomically $
    fold (\ r (_, inChan) -> pure (inChan : r) ) [] (M.stream rmap)
  mapM_ (`writeChan` reply) channels

broadcastOthers :: UniqueRequester -> Requesters s -> s -> IO ()
broadcastOthers requester rmap reply = do
  channels <- atomically $
    fold (\ r (requester', inChan) ->
            if requester' /= requester
            then pure (inChan : r)
            else pure r
         ) [] (M.stream rmap)
  mapM_ (`writeChan` reply) channels

data BackhandChannelStatus
  = NoChannelFound
  | ChannelJoinSuccess
  deriving Generic

instance FromJSON BackhandChannelStatus
instance ToJSON BackhandChannelStatus

data BackhandMessageStatus
  = SendFailure
  | SendSuccess
  deriving Generic

instance FromJSON BackhandMessageStatus
instance ToJSON BackhandMessageStatus
