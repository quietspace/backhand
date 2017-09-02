{-# LANGUAGE TupleSections #-}
module Backhand.Lobby where

import Control.Concurrent.STM
import Backhand.Channel
import Backhand.Unique

type Lobby c s = (Channel c s, ChannelMap c s)

newLobby :: IO (ChanUUID, Lobby c s)
newLobby =
  let toLobby (uid,chan) chmap = (uid, (chan, chmap))
  in pure toLobby <*> newChannel <*> atomically newChannelMap
