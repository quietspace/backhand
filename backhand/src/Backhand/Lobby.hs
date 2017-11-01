{-# LANGUAGE TupleSections #-}

module Backhand.Lobby where

import Data.UUID

import Backhand.Channel

data Lobby t c s = Lobby
  { channel :: Channel t c s
  , channels :: Channels t c s
  }

newLobby :: IO (UUID, Lobby t c s)
newLobby =
  let toLobby (uid,chan) chans = (uid, Lobby chan chans)
  in pure toLobby <*> newChannel <*> newChannelsIO
