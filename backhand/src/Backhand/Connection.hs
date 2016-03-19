module Backhand.Connection
       ( Connection
       , getConnId
       , getUnagi
       , Unagi
       , getInChan
       , writeChan
       , getOutChan
       , readChan
       , newConnection
         -- * Exports
       , InChan
       , OutChan
       ) where

import Control.Concurrent.Chan.Unagi

import Backhand.Unique

newtype Connection c
  = Connection
    { unConnection :: (UniqueConnId, Unagi c) }

getConnId :: Connection c -> UniqueConnId
getConnId = fst . unConnection

getUnagi :: Connection c -> Unagi c
getUnagi = snd . unConnection

newtype Unagi a
  = Unagi
    { unUnagi :: (InChan a, OutChan a) }

getInChan :: Unagi a -> InChan a
getInChan = fst . unUnagi

getOutChan :: Unagi a -> OutChan a
getOutChan = snd . unUnagi

newConnection :: IO (Connection c)
newConnection = pure (curry Connection)
                <*> newConnId
                <*> (pure Unagi <*> newChan)
