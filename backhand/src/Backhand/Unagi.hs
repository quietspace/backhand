

module Backhand.Unagi (
  -- * Unagi
  Unagi,
  newUnagi,
  newUnagiBoundedBy,
  inChan,
  outChan,

  -- * Message
  Messenger(..),
  ) where

import Control.Concurrent.Chan.Unagi.Bounded

-- Unagi --

-- | A representation of a channel from "Control.Concurrent.Chan.Unagi.Bounded".
-- Bounded to limit the usage of memory with the latency of the process.
data Unagi a = Unagi
  { inChan :: InChan a
  , outChan :: OutChan a
  }

newUnagi :: IO (Unagi a)
newUnagi = newUnagiBoundedBy 100

newUnagiBoundedBy :: Int -> IO (Unagi a)
newUnagiBoundedBy n =
  pure (uncurry Unagi) <*> newChan n

-- Message --

class Messenger a where
  sendWith :: a b -> b -> IO ()
  readWith :: a b -> IO b

instance Messenger Unagi where
  sendWith (Unagi inchan _) msg = writeChan inchan msg
  readWith (Unagi _ outchan) = readChan outchan
