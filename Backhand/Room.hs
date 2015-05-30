{-# LANGUAGE Arrows #-}
module Backhand.Room where

import Control.Concurrent.STM
import Control.Monad.Trans.Resource
import Data.Aeson

import Debug.Trace

-- | An empty room.
mkRoom :: Room
mkRoom = Room
    { rClients = []
    , rLastClientId = 0
    }

type ClientId = Integer

-- TODO: Move this stuff to a separate module for dealing with clients.
data Client = Client
    { cId :: ClientId
    , cChan :: TQueue RoomMsg -- ^ Channel for sending messages to the client.
    }

instance Eq Client where
    a == b = cId a == cId b


-- | Container object for a room's state. This holds the "internal state" of the
-- room; stuff internal to Backhand. It also holds the @Auto@ which is
-- responsible for the room's behavior in its interaction with clients.
data Room = Room
    { rClients      :: [Client]
    , rLastClientId :: ClientId
    }
    
-- | True if the given room has no clients connected.
isEmptyRoom :: Room -> Bool
isEmptyRoom = null . rClients

-- | Data structure which represents a message sent from a room to a client.
data RoomMsg = RoomMsg { rMsgObj :: Object }

-- | Data structure which represents a message sent from a client to a room.
data ClientMsg = ClientMsg { cMsgObj :: Object }

-- | Represents a "handle" to a particular room. This is given to clients when
-- they register with the room and is associated with a particular client ID
-- within that room. It can be used to send and receive messages as that client.
-- This object *must* be cleaned up when the client disconnects.
data RoomHandle = RoomHandle
    { rhRecvChan   :: TQueue RoomMsg -- ^ Queue for received messages from the room.
    , rhRoom       :: TVar Room      -- ^ TVar containing the room's state.
    , rhClientId   :: ClientId       -- ^ This client's ID within the room.
    , rhReleaseKey :: ReleaseKey     -- ^ ResourceT release key for this room.
    }


-- | An STM action which joins the given room and returns a @RoomHandle@
-- representing the connection.  The @RoomHandle@ *must* be cleaned up when it
-- is no longer needed. To ensure proper resource handling, this function should
-- not be called directly. Instead, use `Backhand.Core.joinRoom`.
joinRoomSTM :: TVar Room -> STM RoomHandle
joinRoomSTM roomTV = do
    room <- readTVar roomTV
    recvChan <- newTQueue
    let client = Client
                 { cId = rLastClientId room + 1
                 , cChan = recvChan
                 }
    writeTVar roomTV $ room
      { rClients = client : rClients room
      , rLastClientId = rLastClientId room + 1
      }
    printClientCount roomTV
    return RoomHandle
      { rhRecvChan = recvChan
      , rhClientId = cId client
      , rhRoom = roomTV
      , rhReleaseKey = undefined -- This should be set later by the joinRoom
                                 -- function.
      }

-- | An STM action which closes the given room handle, disconnecting the client
-- from the room. Like @joinRoomSTM@, this function is primarily for internal
-- use. `Backhand.Core.partRoom` should be used instead to ensure cleanup is
-- handled properly.
partRoomSTM :: RoomHandle -> STM ()
partRoomSTM (RoomHandle { rhRoom = roomTV, rhClientId = clientId }) = do
    modifyTVar roomTV (\room -> room { rClients = removeClient $ rClients room })
    printClientCount roomTV
  where
    -- Keep only clients with client IDs different from ours.
    removeClient = filter (\c -> cId c /= clientId)


-- | Debugging function.
printClientCount :: TVar Room -> STM ()
printClientCount roomTV = do
    clientCount <- length <$> rClients <$> readTVar roomTV
    trace ("Client count: " ++ show clientCount) $ return ()
