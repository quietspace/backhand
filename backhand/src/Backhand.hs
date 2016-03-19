module Backhand
       ( -- * Aliases
         Connection
       , Unagi
         -- * BMap
       , BMap (..)
       , newBMap
       , addChannel
         -- * Backhand
       , Backhand (..)
       , newBackhand
         -- * Channel
       , Channel (Lobby, Room)
       , chanId
       , chanConnections
       , chanModules
       , newLobby
       , newRoom
       , subchannels
         -- * Unique Identifiers
       , UniqueChanId
       , newChanId
       , UniqueConnId
       , newConnId
       , UniqueModId
       , newModId
         -- * connectiondata
       , ConnectionData (..)
       ) where

import Backhand.Channel
import Backhand.Connection
import Backhand.Unique

-- | A root type for `Channel`.
newtype Backhand c
  = Backhand
    { channels :: BMap c }

-- | Create a backhand service, this additionally initializes a thread that reads a
-- queue that is passed to channels created with the service, so that it can
-- free channels from it's internal map.
newBackhand :: IO (Backhand c)
newBackhand = fmap Backhand newBMap

data ConnectionData c
  = ConnectionData
    { -- | Hashed room unique to allow the connector to know where to send the
      -- message back to.
      roomId  :: UniqueChanId
      -- | Type for modules to filter out if the message is intended for them.
    , modType :: UniqueModId
      -- | Package message for the client/module.
    , message :: c }
