{-# LANGUAGE FlexibleContexts #-}
-- | This module contains functions and data structures related to obtaining and
-- using connections to a room.
module Backhand.Room.Handle
    ( Room
    , module Backhand.Room.Types
    , RoomHandle (rhRoom, rhClientId, rhReleaseKey)
    , msgRoom
    , recvRoomMsg
    -- * Internal Stuff
    , updateClientList
    , joinRoom
    , partRoom
    ) where

import Control.Applicative -- Implicit in GHC 7.10
import Control.Concurrent.STM
import Control.Monad.Trans
import Control.Monad.Trans.Resource

import Backhand.Room.Types
import Backhand.Room.Internal


-- | Represents a "handle" to a particular room. This is given to clients when
-- they register with the room and is associated with a particular client ID
-- within that room. It can be used to send and receive messages as that client.
-- This object *must* be cleaned up when the client disconnects.
data RoomHandle = RoomHandle
    { rhRecvChan   :: TQueue RoomMsg -- ^ Queue for received messages from the room.
    , rhRoom       :: Room           -- ^ The room this handle is connected to.
    , rhClientId   :: ClientId       -- ^ This client's ID within the room.
    , rhReleaseKey :: ReleaseKey     -- ^ ResourceT release key for this room.
    }


-- | Sends a message from a client to the room connected on the given handle.
msgRoom :: (MonadIO m) => RoomHandle -> MsgData -> m ()
msgRoom hand msg =
    -- To handle a message, all we need to do is call the room's `rHandleMsg`
    -- function and the FRP stuff should take over from there.
    liftIO $ rHandleMsg (rhRoom hand) (ClientMsg client msg)
  where
    client = Client (rhClientId hand) (rhRecvChan hand)


-- | STM action which receives a message from the room connected to the given handle.
recvRoomMsg :: RoomHandle -> STM RoomMsg
recvRoomMsg hand = do
    readTQueue $ rhRecvChan hand


-- | Joins the given room and returns a @RoomHandle@ representing the
-- connection. The @RoomHandle@ *must* be explicitly destroyed via the
-- `partRoom` function when it is no longer needed. Otherwise, the room will not
-- be notified that the client has disconnected. It is not enough to simply let
-- the garbage collector take care of the handle. This function is primarily for
-- internal use. `Backhand.Core.joinCoreRoom` should be used instead to ensure
-- cleanup is handled properly.
joinRoom :: (MonadIO m, MonadBaseControl IO m) => Room -> m RoomHandle
joinRoom room = withRoomLocked room $ do
    hand <- liftIO $ atomically mkHandle
    liftIO $ updateClientList room
    return hand
  where
    mkHandle = do
      recvChan <- newTQueue
      clientId <- readTVar $ rNextClientId room
      let client = Client
                   { cId = clientId
                   , cChan = recvChan
                   }
      modifyTVar (rClients room) (client:)
      modifyTVar (rNextClientId room) (+1)
      return RoomHandle
               { rhRecvChan = recvChan
               , rhClientId = cId client
               , rhRoom = room
               , rhReleaseKey = undefined -- This should be set later by the
                                          -- joinCoreRoom function.
               }

-- | Closes the given room handle and disconnects the client from the room. This
-- function is primarily for internal use. `Backhand.Core.partCoreRoom` should
-- be used instead to ensure cleanup is handled properly.
partRoom :: (MonadIO m, MonadBaseControl IO m) => RoomHandle -> m ()
partRoom hand@(RoomHandle { rhRoom = room }) = withRoomLocked room $ do
    -- TODO: Somehow invalidate the room handle so it cannot be used after this.
    liftIO $ atomically $ modifyTVar (rClients room) removeClient
    liftIO $ updateClientList room
  where
    -- Keep only clients with client IDs different from ours.
    removeClient = filter (\c -> cId c /= rhClientId hand)
