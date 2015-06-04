-- | Internal plumbing for rooms.
module Backhand.Room.Internal where

import Control.Applicative -- Implicit in GHC 7.10
import Control.Concurrent.STM
import Reactive.Banana.Frameworks

import GHC.Conc.Sync

import Backhand.Room.Types
import Backhand.Room (RoomBehavior)

-- | Creates a new room with the given event handler and initial state.
mkRoom :: RoomId -> RoomBehavior -> STM Room
mkRoom roomId behavior = do
  clients <- newTVar []
  nextCId <- newTVar 0
  -- Unsafe IO is necessary here to keep this function as an STM action. This
  -- shouldn't cause problems, as constructing an event network shouldn't have
  -- any real side-effects. If this action happens to be performed multiple
  -- times, previously-compiled networks should simply be garbage collected and
  -- never used, as they don't really do much unless the handlers are called.
  (handleMsg, handleClientsChange) <- unsafeIOToSTM $ do
    (addMsgEvt, handleMsg) <- newAddHandler
    (addClientListEvt, handleClientsChange) <- newAddHandler
    network <- behavior addMsgEvt addClientListEvt
    actuate network
    return (handleMsg, handleClientsChange)
  return Room
         { rId = roomId
         , rClients = clients
         , rNextClientId = nextCId
         , rHandleMsg = handleMsg
         , rHandleClients = handleClientsChange
         }

-- | Container object for a room's state. This holds the "internal state" of the
-- room; stuff internal to Backhand. It also holds the room's message handler
-- and the state for that handler.
data Room = Room
    { rId            :: RoomId
    , rClients       :: TVar [Client]
    , rNextClientId  :: TVar ClientId
    , rHandleMsg     :: ClientMsg -> IO () -- ^ IO action to fire the client
                                           -- message event.
    , rHandleClients :: [Client] -> IO () -- ^ IO action to let the behavior
                                          -- know the client list changed.
    }

-- | True if the given room has no clients connected.
isEmptyRoom :: Room -> STM Bool
isEmptyRoom = fmap null . readTVar . rClients  

-- | Sends the room's behavior a copy of the room's current client list. This
-- should be called after any call to `joinRoomSTM` or `partRoomSTM`.
updateClientList :: Room -> IO ()
updateClientList room = do
    cs <- readTVarIO (rClients room)
    rHandleClients room cs

