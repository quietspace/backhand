{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Backhand.Room.Monad where

import Prelude

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad.Trans

import Backhand.Room.Types

class (Monad m, Applicative m) => MonadRoom m where
    -- | Sends a message to the given client.
    sendMessage :: Client -> MsgData -> m ()

newtype RoomM a = RoomM { unRoomM :: IO a }
    deriving (Functor, Applicative, Monad)

instance MonadRoom RoomM where
    sendMessage client msg = RoomM $ liftIO $
        atomically $ writeTQueue (cChan client) msg

                   
