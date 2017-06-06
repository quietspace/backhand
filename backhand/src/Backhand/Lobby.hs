{-# LANGUAGE TupleSections #-}
module Backhand.Lobby where

import Backhand.Channel
import Backhand.Unique

type Lobby c = (Channel c, ChannelMap c)

newLobby :: IO (UniqueChanId, Lobby c)
newLobby =
    let toLobby (uid,chan) chmap = (uid, (chan, chmap))
    in pure toLobby <*> newChannel <*> newChannelMap
