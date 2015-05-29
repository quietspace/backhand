module Backhand.Room where

import Control.Concurrent.STM
import Control.Monad.Trans.Resource

-- Placeholders
class Room r where
    -- | Returns an initial state object for the room.
    initRoom :: r
    -- | True if the given room has no clients connected.
    isEmptyRoom :: r -> Bool

data RoomMsg

-- | Represents a "handle" to a particular room. This is given to clients when
-- they register with the room and is associated with a particular client ID
-- within that room. It can be used to send and receive messages as that client.
-- This object *must* be cleaned up when the client disconnects.
data (Room r) => RoomHandle r = RoomHandle
    { rhRecvChan   :: TQueue RoomMsg -- ^ Queue for received messages from the room.
    , rhRoom       :: TVar r         -- ^ TVar containing the room's state.
    , rhReleaseKey :: ReleaseKey     -- ^ ResourceT release key for this room.
    }


-- | An STM action which joins the given room and returns a @RoomHandle@
-- representing the connection.  The @RoomHandle@ *must* be cleaned up when it
-- is no longer needed. To ensure proper resource handling, this function should
-- not be called directly. Instead, use `Backhand.Core.joinRoom`.
joinRoomSTM :: (Room r) => TVar r -> STM (RoomHandle r)
joinRoomSTM room = do
    -- TODO: Somehow notify the room we're joining. Rooms haven't been
    -- implemented yet, so as a placeholder, we'll just make a fake TQueue.
    recvChan <- newTQueue
    return RoomHandle
      { rhRecvChan = recvChan
      , rhRoom = room
      , rhReleaseKey = undefined -- This should be set later by the joinRoom
                                 -- function.
      }

-- | An STM action which closes the given room handle, disconnecting the client
-- from the room. Like @joinRoomSTM@, this function is primarily for internal
-- use. `Backhand.Core.partRoom` should be used instead to ensure cleanup is
-- handled properly.
partRoomSTM :: (Room r) => RoomHandle r -> STM ()
partRoomSTM roomHand = do
    return () -- Nothing to do here yet.
