{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
-- | This module implements Backhand's "core" object---the data structure which
-- contains the list of rooms.
module Backhand.Core
    ( BackhandCore
    , initCore
    , joinCoreRoom
    , partCoreRoom
    ) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Monoid

import Backhand.Room.Handle
import Backhand.Room.Internal
import Backhand.Behavior.Chat
import Backhand.Behavior.Reversi


-- | The object which holds a TVar containing the list of rooms present on this
-- server. Each entry in the list is a room object which contains TVars storing
-- the state of the room.
--
-- This is essentially Backhand's core state object. The rooms list is accessed
-- by each connection thread individually.
data BackhandCore = BackhandCore
    { coreRooms :: TVar (Map RoomId Room) -- ^ Map of room IDs to rooms.
    , coreMkRoom :: RoomId -> STM Room
    }

-- | Initializes a new @BackhandCore@ object.
initCore :: IO BackhandCore
initCore = do
    rooms <- newTVarIO M.empty
    return BackhandCore
        { coreRooms = rooms
        , coreMkRoom = \roomId -> mkRoom roomId (chatRoomBehavior <> reversiBehavior)
        }


-- | STM action which finds the room with the given ID if it exists. If the room
-- does not exist, the action will retry.
findRoomSTM :: BackhandCore -> RoomId -> STM Room
findRoomSTM core roomId = do
    rooms <- readTVar (coreRooms core)
    when (roomId `M.notMember` rooms) retry
    return (rooms M.! roomId)

-- | STM action which creates a new room with the given ID and adds it to the
-- given core's room list. Returns a @TVar@ with the newly created room in it.
-- If a room with the given ID already exists, this action will retry.
createRoomSTM :: BackhandCore -> RoomId -> STM Room
createRoomSTM core roomId = do
    rooms <- readTVar $ coreRooms core
    when (roomId `M.member` rooms) $ fail "Room already exists."
    room <- coreMkRoom core roomId
    modifyTVar (coreRooms core) (M.insert roomId room)
    return room

-- | STM action which removes the given room from the core's room list.
destroyRoomSTM :: BackhandCore -> Room -> STM ()
destroyRoomSTM core room = do
    isEmpty <- isEmptyRoom room
    unless isEmpty $ fail "Not Implemented: Can only remove empty rooms."
    modifyTVar (coreRooms core) (M.filter (\r -> rId r /= rId room))

-- TODO: Disconnect clients when room is destroyed. For now, we'll just throw an
-- error if anyone is still in a room, since we have no reason to remove
-- non-empty rooms yet.

-- | Join a room with the given ID. If one does not exist, it will be created.
-- This involves constructing a "handle" object to represent the connection to
-- the room.  The client *must* notify the room when it is disconnecting,
-- otherwise Bad Things (TM) will happen. To prevent this situation, this
-- function must run inside of a ResourceT monad.
joinCoreRoom :: (MonadIO m, MonadResource m, MonadBaseControl IO m) =>
                BackhandCore -> RoomId -> m RoomHandle
joinCoreRoom core roomId = setReleaseKey <$> allocate doJoin doExit
  where
    -- Takes a tuple of release key and room handle and returns the room handle
    -- with the key stored inside it.
    setReleaseKey (rk, rh) = rh { rhReleaseKey = rk }
    doExit = partCoreRoom core
    doJoin = do
      -- We need to lock the room we're joining because joining a room requires
      -- running an IO action and we need to guarantee no other thread will do
      -- something in the time between us finding the room and joining it.
      room <- atomically $ do
                r <- findRoomSTM core roomId <|> createRoomSTM core roomId
                lockRoom r
                return r
      hand <- joinRoom room
      atomically $ unlockRoom room
      return hand

-- | Disconnects the given room handle. If the room is empty after
-- disconnecting, it will be removed from existence.
partCoreRoom :: (MonadIO m, MonadBaseControl IO m) => BackhandCore -> RoomHandle -> m ()
partCoreRoom core hand = do
    partRoom hand
    removeIfEmpty
    -- Ensure the room handle is no longer protected by the resource monad. This
    -- prevents accidental duplicate disconnects.
    _ <- unprotect (rhReleaseKey hand)
    return ()
  where
    removeIfEmpty = liftIO $ atomically $ do
      isEmpty <- isEmptyRoom $ rhRoom hand
      when isEmpty $ destroyRoomSTM core (rhRoom hand)
