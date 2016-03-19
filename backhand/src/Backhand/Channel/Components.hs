module Backhand.Channel.Components where

import Control.Concurrent.Chan.Unagi
import Control.Concurrent.STM
import STMContainers.Map

import Backhand.Connection
import Backhand.Unique

data Components c
  = Components
    { -- | Unique identifier for a channel.
      uid         :: UniqueChanId
      -- | List of connections associated with the channel.
    , connections :: Map UniqueConnId (InChan c)
      -- | List of modules that they channel is handling.
    , modules     :: Map UniqueModId (InChan c) }

newComponents :: [Connection c] -> IO (Components c)
newComponents cs = pure Components <*> newChanId <*> newConnMap <*> newIO
  where
    newConnMap = atomically $ do
      c <- new
      mapM_ (\ conn -> insert (getInChan $ getUnagi conn) (getConnId conn) c) cs
      pure c
