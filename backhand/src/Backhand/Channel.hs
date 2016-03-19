module Backhand.Channel
       ( BMap (..)
       , newBMap
       , addChannel
       , Channel (Lobby, Room)
       , chanId
       , chanConnections
       , chanModules
       , newLobby
       , newRoom
       , subchannels
       ) where

import Control.Concurrent.STM
import STMContainers.Map           as M

import Backhand.Channel.Components
import Backhand.Connection
import Backhand.Unique

newtype BMap c
  = BMap
    { unBMap :: Map UniqueChanId (Channel c) }

newBMap :: IO (BMap c)
newBMap = fmap BMap newIO

addChannel :: Channel c -> UniqueChanId -> BMap c -> IO ()
addChannel c cid bmap = atomically $ M.insert c cid (unBMap bmap)

-- | Session instances that has modules for functionality
data Channel c
    -- | A lobby can contain many instances of rooms, as well as connections and
    -- modules.
  = Lobby
    { -- | CAUTION: Partial function that will error out on `Room` use `subchannels` for a `Maybe` wrapper.
      unsafeSubchannels :: BMap c
      -- | Various STM variables that a channel uses to manage basic behavior.
    , components        :: Components c }

    -- | A room can only contain connections and modules.
  | Room
    { components :: Components c }

chanId :: Channel c -> UniqueChanId
chanId = uid . components

chanConnections :: Channel c -> Map UniqueConnId (InChan c)
chanConnections = connections . components

chanModules :: Channel c -> Map UniqueModId (InChan c)
chanModules = modules . components

newLobby :: [Connection c] -> IO (Channel c)
newLobby cs = pure Lobby <*> newBMap <*> newComponents cs

newRoom :: [Connection c] -> IO (Channel c)
newRoom cs = pure Room <*> newComponents cs

subchannels :: Channel c -> Maybe (BMap c)
subchannels a@(Lobby {}) = Just (unsafeSubchannels a)
subchannels _ = Nothing
