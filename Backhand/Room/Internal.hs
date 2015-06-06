{-# LANGUAGE FlexibleContexts #-}
-- | Internal plumbing for rooms.
module Backhand.Room.Internal where

import Prelude hiding ((.))

import Control.Applicative -- Implicit in GHC 7.10
import Control.Concurrent.STM
import Control.Concurrent.STM.TRLock
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Wire
import Control.Wire.Unsafe.Event

import Backhand.Room.Types
import Backhand.Room.Monad
import Backhand.Room (RoomBehavior)

-- | Creates a new room with the given event handler and initial state.
mkRoom :: RoomId -> RoomBehavior -> STM Room
mkRoom roomId behavior = do
  clients <- newTVar []
  nextCId <- newTVar $ ClientId 0
  behTVar <- newTVar behavior
  lock <- newTRLock
  return Room
         { rId = roomId
         , rClients = clients
         , rNextClientId = nextCId
         , rBehavior = behTVar
         -- , rHandleMsg = handleMsg
         -- , rHandleClientEvt = handleClientEvt
         , rLock = lock
         }

-- | Container object for a room's state. This holds the "internal state" of the
-- room; stuff internal to Backhand. It also holds the room's message handler
-- and the state for that handler.
data Room = Room
    { rId              :: RoomId
    , rClients         :: TVar [Client]
    , rNextClientId    :: TVar ClientId
    , rBehavior        :: TVar RoomBehavior
    , rLock            :: TRLock -- ^ Mutex lock for this room. This is locked
                                 -- for non-STM actions such as joining,
                                 -- parting, and handling messages into order to
                                 -- prevent race conditions.
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


-- | Takes a client event and steps the room's behavior.
handleEvent :: (MonadIO m, MonadBaseControl IO m) => Room -> ClientEvent -> m ()
handleEvent room evt = withRoomLocked room $ do
    -- It is safe to not use an STM transaction here since the room is locked.
    oldWire <- liftIO $ readTVarIO $ rBehavior room
    (_, newWire) <- liftIO $ unRoomM $ stepWire oldWire () (Right $ Event evt)
    liftIO $ atomically $ writeTVar (rBehavior room) newWire
