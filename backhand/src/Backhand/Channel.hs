module Backhand.Channel where

import qualified STMContainers.Map as M

import Control.Concurrent.Chan.Unagi.Bounded
import Control.Concurrent.STM
import Data.Semigroup.Bitraversable

import Backhand.Unique

-- * Types

type Requesters c = M.Map UniqueRequesterId (InChan c)
type Modules c = M.Map UniqueModuleId (InChan c)

-- ** Channel

-- | Our product type for channel communications. Requestors are your abstract
-- clients; you will most likely want those clients to be managed and act as the
-- middleman for your providers. Modules are your abstract services which can
-- be anything from a game, a chat server, a client to another server, etc.
type Channel c = (Requesters c, Modules c)

-- | Helper function to help you get started.
newChannel :: IO (UniqueChanId, Channel c)
newChannel = bitraverse1 id bisequence1 (newChanId, (M.newIO, M.newIO))

-- ** ChannelMap

-- | The ChannelMap is the backbone of our routing type. This will be the
-- structure you interact with to start new connections, delete old channels by
-- their `UniqueChanId`, and ultimately any action that has to do with creating
-- a new channel.
type ChannelMap c = M.Map UniqueChanId (Channel c)

-- | Helper function to help you get started.
newChannelMap :: IO (ChannelMap c)
newChannelMap = M.newIO

-- * Utility

-- | Interacting with `Channel`s and `ChannelMap`s are the reason this library
-- exists so we provide some very basic functions for interactions between them.
-- You will want to wrap these in your own logic; such as creating a new
-- chatroom you'll add that chatroom instance (in this case a `Channel`) to the
-- chat server (in this case the `ChannelMap`).

addChannel :: UniqueChanId -> Channel c -> ChannelMap c -> STM ()
addChannel u (r, m) map' = M.insert (r, m) u map'

addChannel' :: UniqueChanId -> Channel c -> ChannelMap c -> IO ()
addChannel' u ch chm = atomically $ addChannel u ch chm

addNewChannel :: ChannelMap c -> IO UniqueChanId
addNewChannel chm = do
    (u, ch) <- newChannel
    addChannel' u ch chm
    pure u

delChannel :: UniqueChanId -> ChannelMap c -> STM ()
delChannel cid map' = M.delete cid map'

delChannel' :: UniqueChanId -> ChannelMap c -> IO ()
delChannel' cid chm = atomically $ delChannel cid chm
