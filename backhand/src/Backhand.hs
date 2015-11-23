{-# LANGUAGE FlexibleInstances #-}

module Backhand
       ( -- * Type aliases
         BMap
         -- * Main types
       , Backhand (..)
       , Channel (Lobby, Room, uid, connections, modules)
       , UniqueId (..)
       , ConnectionId (..)
         -- * Main classes
       , MessagePass (..)
       , Workers (..)
         -- * Main functions
       , spawnBackhand
       , spawnRoom
       , spawnRoomIn
         -- ** Operation types
       , ChannelOperations (..)
       , ConnectionOperations (..)
       , ModuleOperations (..)
         -- ** Helper functions
       , subchannels
       , addConnectionToChannel
       , removeConnectionFromChannel
       , addModuleToChannel
       , mkRoom
       , mkEmptyRoom
       , mkLobby
       , mkEmptyLobby
       ) where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Data.Hashable
import           STMContainers.Map

-- | A thread-safe map for managing `Channel` threads and queues. The `TQueue`
-- allows us to talk to a Channel's worker that we want to pass a connection for
-- it to manage.
type BMap c = STM (Map UniqueId (ThreadId, TQueue (ConnectionOperations c)))

-- | A typeclass interface for data types with worker threads.
class Workers a where
  workers :: a -> [ThreadId]

-- | An intentionally simple toplevel type that should serve well for most use
-- cases. If an user needs something specific then they can make a type that wraps
-- `BMap` and make their own startup function as everything else just requires
-- `BMap`.
data Backhand c
  = Backhand
    { -- | A map of TQueues which are read by Channel threads.
      channels        :: BMap c
      -- | Internal queue to pass to sub-channel threads.
    , backhandQueue   :: TQueue ChannelOperations
      -- | Internal list of worker threads.
    , workersBackhand :: [ThreadId] }

instance Workers (Backhand c) where
  workers = workersBackhand

data Channel c m
  -- | A lobby can contain many instances of rooms, as well as connections and
  -- modules.
  = Lobby
    { -- | Unique identifier for the channel.
      uid               :: UniqueId
      -- Lobbies manage their own channels for more intricate behavior.
      -- unsafeSubchannels is a partial function, we expose subchannels as an
      -- alternative that works with all Channels using Maybe.
    , unsafeSubchannels :: BMap c
      -- | Each connection and queue for sending messages.
    , connections       :: TVar [(ConnectionId, TQueue c)]
      -- | Queue for message passing to modules.
    , modules           :: TVar [TQueue m]
      -- | Internal queue to pass to sub-channel threads.
    , channelQueue      :: TQueue ChannelOperations
      -- | Internal list of worker threads.
    , workersChannel    :: [ThreadId] }

  -- | A room can only contain connections and modules.
  | Room
    { uid         :: UniqueId
    , connections :: TVar [(ConnectionId, TQueue c)]
    , modules     :: TVar [TQueue m] }

instance Workers (Channel c m) where
  workers a@(Lobby {}) = workersChannel a
  workers _ = []

subchannels :: Channel c m -> Maybe (BMap c)
subchannels a@(Lobby {}) = Just (unsafeSubchannels a)
subchannels _ = Nothing

-- TODO: Need to make a system that actually handles the random and uniqueness
-- that this library should have. For now there is a simple newtype wrapper
-- around Integer.
newtype UniqueId
  = UIDInteger Integer
  deriving (Eq)

instance Hashable UniqueId where
  hashWithSalt int (UIDInteger uint) = hashWithSalt int uint

newtype ConnectionId
  = CIDInteger Integer
  deriving (Eq)

addConnectionToChannel :: Channel c m -> (ConnectionId, TQueue c) -> STM ()
addConnectionToChannel channel connection
  = modifyTVar (connections channel) (connection :)

removeConnectionFromChannel :: Channel c m -> ConnectionId -> STM ()
removeConnectionFromChannel channel cid
  = modifyTVar (connections channel) removeConnection
  where
    removeConnection :: [(ConnectionId, TQueue c)] -> [(ConnectionId, TQueue c)]
    removeConnection [] = []
    removeConnection (x@(a,_):xs) | a == cid = xs
                                  | otherwise = x : removeConnection xs

addModuleToChannel :: Channel c m -> TQueue m -> STM ()
addModuleToChannel channel module'
  = modifyTVar (modules channel) (module' :)

data ConnectionOperations c
  -- Joining a channel, if it already exists.
  = JOIN        (ConnectionId, TQueue c)
  -- Creating a channel that doesn't already exist.
  | CREATELOBBY (ConnectionId, TQueue c)
  | CREATEROOM  (ConnectionId, TQueue c)

-- | Internal operation type for notifying `Backhand` or `Lobby` when a subchannel closes.
data ChannelOperations
  = CLOSE UniqueId -- ^ Remove the channel from a higher system.

data ModuleOperations m
  = MsgClient ConnectionId m
  | MsgClients [ConnectionId] m
  | MsgAllClients m

class MessagePass c m where
  toClient :: m -> c
  toModule :: c -> m

-- | Creating a case where a user wants to use a single data type for both
-- Connection and Module instances, it can be properly supported by just passing
-- the type back and forth.
instance MessagePass a a where
  toClient = id
  toModule = id

-- | Create a backhand service, this additionally spawns a thread that reads a
-- queue that is passed to channels created with the service, so that it can
-- free channels from it's internal map.
spawnBackhand :: IO (Backhand c)
spawnBackhand = do
  bmap <- pure new
  backhandQueue' <- atomically newTQueue
  worker <- forkIO (forever (actOnChannelQueue bmap backhandQueue'))
  pure (Backhand bmap backhandQueue' [worker])

-- | Spawn a Room thread and add it to the `BMap`.
spawnRoom :: BMap c -> [TQueue m] -> IO ()
spawnRoom bmap queue = do

  pure ()

-- | `spawnRoom` wrapper for `Backhand`.
spawnRoomIn :: Backhand c -> [TQueue m] -> IO ()
spawnRoomIn b queue = spawnRoom (channels b) queue

tempRoom :: BMap c -> IO ()
tempRoom bmap = do

  pure ()

actOnQueue :: (a -> STM ()) -> TQueue a -> IO ()
actOnQueue fn q = atomically (readTQueue q >>= fn)

-- | Simple STM based action to handle reading from backhand's channel queue and
-- act upon it's internal map accordingly.
--
-- At this time it just removes the channel's queue.
actOnChannelQueue :: BMap c -> TQueue ChannelOperations -> IO ()
actOnChannelQueue bmap = actOnQueue chanOp
  where
    chanOp :: ChannelOperations -> STM ()
    chanOp (CLOSE k) = bmap >>= delete k

-- | Generating a room is a common pattern that can be generalized.
mkRoom :: UniqueId -> [(ConnectionId, TQueue c)] -> [TQueue m] -> IO (Channel c m)
mkRoom uid' connections' modules' = atomically $ do
  connectionPool <- newTVar connections'
  moduleList <- newTVar modules'
  pure (Room uid' connectionPool moduleList)

-- | For simple testing, and patterns where you want to create a Room with no
-- connections or modules.
mkEmptyRoom :: UniqueId -> IO (Channel c m)
mkEmptyRoom uid' = mkRoom uid' [] []

-- | Generating a lobby is much like a room and this function just gives it an empty `BMap`.
mkLobby :: UniqueId -> [(ConnectionId, TQueue c)] -> [TQueue m] -> IO (Channel c m)
mkLobby uid' connections' modules' = do
  connectionPool <- atomically $ newTVar connections'
  moduleList <- atomically $ newTVar modules'
  bmap <- pure new
  channelQueue' <- atomically newTQueue
  worker <- forkIO (forever (actOnChannelQueue bmap channelQueue'))
  pure (Lobby uid' bmap connectionPool moduleList channelQueue' [worker])

-- | For simple testing, and patterns where you want to create a Lobby with no
-- connections or modules.
mkEmptyLobby :: UniqueId -> IO (Channel c m)
mkEmptyLobby uid' = mkLobby uid' [] []
