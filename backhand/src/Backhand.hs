module Backhand
-- * Aliases
  ( Unagi
   -- * Channel
  , Channel
  , newChannel
  , ChannelMap
  , newChannelMap
  , newChannelMapIO
  , addChannel
  , addChannel'
  , addNewChannel
  , joinChannel
  , joinChannel'
  , leaveChannel
  , sendMessage
  , findChannel
  , isChannelPresent
  , broadcast
  , broadcastOthers
  , delChannel
  , delChannel'
  , Requesters
  , newRequester
  , Modules
  , newModule
   -- * Lobby
  , Lobby
  , newLobby
   -- * Unique Identifiers
  , ChanUUID(..)
  , newChanUUID
  , UniqueRequester
  , newRequesterId
   -- * Message types
  , Message
  , ConnectionData(..)
   -- * Status
  , BackhandChannelStatus(..)
  , BackhandMessageStatus(..)
  ) where

import Backhand.Channel
import Backhand.Lobby
import Backhand.Message
import Backhand.Unique
