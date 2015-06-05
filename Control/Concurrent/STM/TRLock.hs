{-# LANGUAGE FlexibleContexts #-}
-- | A transactional reentrant lock implementation using STM.
module Control.Concurrent.STM.TRLock
    ( TRLock
    , newTRLock
    -- * Basic Locking
    , acquireTRLock
    , releaseTRLock
    , forceReleaseTRLock
    , unlockTRLock
    -- * High-level Convenience Functions
    , withTRLock
    , withTRLockSTM
    ) where

import Control.Applicative -- Implicit in GHC 7.10
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Exception.Lifted

import GHC.Conc.Sync

-- TODO: Split this off into a separate library.

-- | A transactional reentrant mutual exclusion lock.
-- This is a resource which can be either "locked" or "unlocked" by one thread
-- at a time. This lock is reentrant, which means that if the same thread tries
-- to lock it again after it has already been locked, it will not cause a
-- deadlock. Rather, it will increment a counter within the lock. When it is
-- unlocked, the counter will be decremented. If the counter reaches zero, the
-- mutex will unlock.
data TRLock = TRLock (TVar (Maybe ThreadId, Int))


-- | Creates a new `RLock`.
newTRLock :: STM TRLock
newTRLock = TRLock <$> newTVar (Nothing, 0)

-- | Acquires the lock on the current thread. If another thread has the lock,
-- this will retry. If this thread has the lock, it will be re-locked. The lock
-- will only unlock after the thread has called `releaseTRLock` the same number
-- of times as `acquireTRLock`.
acquireTRLock :: TRLock -> STM ()
acquireTRLock (TRLock lock) = do
    myTId <- unsafeIOToSTM myThreadId
    (owner, _) <- readTVar lock
    case owner of
      Just otherTId -> if myTId == otherTId
                          then modifyTVar lock $ \(o, c) -> (o, c+1)
                          else retry
      Nothing -> writeTVar lock (Just myTId, 1)

-- | Releases the lock once. To unlock the `TRLock`, this must be called exactly
-- as many times as `acquireTRLock` was called. Note: This function will fail
-- with an error if the lock is not owned by this thread. To avoid this, use
-- `forceReleaseTRLock`. If the lock is not locked, this function does nothing.
releaseTRLock :: TRLock -> STM ()
releaseTRLock l@(TRLock lock) = do
    myTId <- unsafeIOToSTM myThreadId
    (owner, _) <- readTVar lock
    case owner of
      Just otherTId -> if myTId == otherTId
                          then forceReleaseTRLock l
                          else fail "Lock is not owned by the calling thread."
      _ -> return ()

-- | This function works similarly to `releaseTRLock`, but it does not check to
-- ensure the lock is owned by the current thread. Use this with care.
forceReleaseTRLock :: TRLock -> STM ()
forceReleaseTRLock (TRLock lock) = modifyTVar lock $ \(owner, ctr) ->
    if ctr - 1 <= 0
       then (Nothing, 0)
       else (owner, ctr - 1)

-- | Forces the TRLock into an unlocked state. This function is pretty dangerous
-- and you probably shouldn't need it, but it's here if the need arises. Note
-- that this does mean that any other thread currently using the lock will end
-- up calling `releaseTRLock` and likely crashing due to the lock having been
-- acquired by another thread.
unlockTRLock :: TRLock -> STM ()
unlockTRLock (TRLock lock) = writeTVar lock (Nothing, 0)


-- | Convenience function which locks the `TRLock`, runs the given IO action,
-- and then unlocks the `TRLock`. This is by far the safest and easiest way to
-- use a lock, so it is highly recommended, as it should make it impossible to
-- "forget" to release the lock due to programmer negligence or exceptions. Note
-- that this is basically equivalent to calling bracket_ with `acquireLock` and
-- `releaseLock` as the acquire and release actions.
withTRLock :: (MonadIO m, MonadBaseControl IO m) => TRLock -> m a -> m a
withTRLock lock =
    bracket_ (liftIO $ atomically $ acquireTRLock lock)
             (liftIO $ atomically $ releaseTRLock lock)

-- | Similar to `withTRLock`, but runs inside an @STM@ action instead of @IO@.
-- Note that this function doesn't currently handle exceptions, so if your STM
-- action terminates unexpectedly, the lock may be left locked.
withTRLockSTM :: TRLock -> STM a -> STM a
withTRLockSTM lock action = do
    -- TODO: Handle exceptions and ensure the lock is released.
    acquireTRLock lock
    a <- action
    releaseTRLock lock
    return a
