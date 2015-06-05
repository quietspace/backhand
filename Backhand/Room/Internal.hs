{-# LANGUAGE FlexibleContexts #-}
-- | Internal plumbing for rooms.
module Backhand.Room.Internal where

import Control.Applicative -- Implicit in GHC 7.10
import Control.Monad
import Control.Monad.Trans.Control
import Control.Concurrent.STM
import Control.Concurrent.STM.TRLock
import Reactive.Banana.Frameworks

import GHC.Conc.Sync

import Backhand.Room.Types
import Backhand.Room (RoomBehavior)

-- | Creates a new room with the given event handler and initial state.
mkRoom :: RoomId -> RoomBehavior -> STM Room
mkRoom roomId behavior = do
  clients <- newTVar []
  nextCId <- newTVar $ ClientId 0
  lock <- newTRLock
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
         , rLock = lock
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
    , rLock          :: TRLock -- ^ Mutex lock for this room. This is locked for
                               -- non-STM actions such as joining, parting, and
                               -- handling messages into order to prevent race
                               -- conditions.
    }

-- | True if the given room has no clients connected.
isEmptyRoom :: Room -> STM Bool
isEmptyRoom = fmap null . readTVar . rClients


-- | Locks the given room. If the room is already locked, retries.
lockRoom :: Room -> STM ()
lockRoom = acquireTRLock . rLock

-- | Locks the given room. If the room is already locked, retries.
unlockRoom :: Room -> STM ()
unlockRoom = releaseTRLock . rLock

-- Returns true if the given room is currently locked.
-- TODO
-- isRoomLocked :: Room -> STM Bool
-- isRoomLocked room = readTVar (rLock room)

-- | Performs the given IO action with the room locked. The room will
-- automatically be unlocked afterward. Roughly equivalent to
-- `atomically (lockRoom r) >> action >> atomically (unlockRoom r)`.
withRoomLocked :: (MonadIO m, MonadBaseControl IO m) => Room -> m a -> m a
withRoomLocked room action = withTRLock (rLock room) action


-- | Sends the room's behavior a copy of the room's current client list.
updateClientList :: Room -> IO ()
updateClientList room = do
    cs <- readTVarIO (rClients room)
    rHandleClients room cs
