{-# LANGUAGE OverloadedStrings #-}
-- | This module implements Backhand's "core" object---the data structure which
-- contains the list of rooms.
module Backhand.Core
    ( BackhandCore
    , initCore
    , joinRoom
    , partRoom
    ) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Backhand.Room.Handle
import Backhand.Room.Internal
import Backhand.Behavior.Chat


-- | The object which holds a TVar containing the list of rooms present on this
-- server. Each entry in the list is a room object which contains TVars storing
-- the state of the room.
--
-- This is essentially Backhand's core state object. The rooms list is accessed
-- by each connection thread individually.
data BackhandCore = BackhandCore
    { coreRooms :: TVar (Map T.Text Room) -- ^ Map of room IDs to rooms.
    , coreMkRoom :: RoomId -> STM Room
    }

-- | Initializes a new @BackhandCore@ object.
initCore :: IO BackhandCore
initCore = do
    rooms <- newTVarIO M.empty
    return BackhandCore
        { coreRooms = rooms
        , coreMkRoom = \roomId -> mkRoom roomId chatRoomBehavior
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

-- | Joins the current thread to a room. This involves associating the current
-- thread with a @TChan@ and a client ID within the target room's state object.
-- The client *must* notify the room when it is disconnecting, otherwise Bad
-- Things (TM) will happen. To prevent this situation, this function must run
-- inside of a ResourceT monad.
--
-- The function returns a @RoomHandle@ which can be used to interface with the
-- room and a @ReleaseKey@ which can be used with the resource monad to
-- disconnect from the room.
joinRoom :: (MonadResource m) => BackhandCore -> RoomId -> m RoomHandle
joinRoom core roomId =
    setReleaseKey <$> allocate doJoin doExit
  where
    -- Takes a tuple of release key and room handle and returns the room handle
    -- with the key stored inside it.
    setReleaseKey (rk, rh) = rh { rhReleaseKey = rk }
    doJoin = do
      (room, hand) <- atomically (joinExisting <|> createAndJoin)
      updateClientList room
      return hand
    joinExisting = findRoomSTM core roomId >>= (\r -> (,) r <$> joinRoomSTM r)
    createAndJoin = createRoomSTM core roomId >>= (\r -> (,) r <$> joinRoomSTM r)
    doExit = partRoom core

-- | Disconnects the given room handle. If the room is empty after
-- disconnecting, it will be removed from existence.
partRoom :: (MonadIO m) => BackhandCore -> RoomHandle -> m ()
partRoom core roomHand = do
    liftIO $ doPart
    liftIO $ updateClientList (rhRoom roomHand)
    -- Ensure the room handle is no longer protected by the resource monad. This
    -- prevents accidental duplicate disconnects.
    _ <- unprotect (rhReleaseKey roomHand)
    return ()
  where
    doPart = atomically $ do
        -- TODO: Somehow invalidate the room handle so it cannot be used after this.
        partRoomSTM roomHand
        isEmpty <- isEmptyRoom $ rhRoom roomHand
        when isEmpty $ destroyRoomSTM core (rhRoom roomHand)
