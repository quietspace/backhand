module Backhand
-- * Aliases
  ( Requester
  , Unagi
   -- * Channel
  , Channel
  , newChannel
  , ChannelMap
  , newChannelMap
  , addChannel
  , addChannel'
  , delChannel
  , delChannel'
   -- * Lobby
  , Lobby
  , newLobby
   -- * Unique Identifiers
  , UniqueChanId
  , newChanId
  , UniqueRequesterId
  , newRequesterId
  , UniqueModuleId
  , newModuleId
   -- * Message types
  , Message
  , ConnectionData(..)
  ) where

import Backhand.Channel
import Backhand.Lobby
import Backhand.Message
import Backhand.Requester
import Backhand.Unique
