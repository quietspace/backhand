{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Backhand.Modules.Lobby.Chat where

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Control.Concurrent.Chan.Unagi as U

import Backhand
import Backhand.Message
import Control.Concurrent.STM
import Data.Hashable
import Data.Unique

startLobbyChat
    :: TVar (M.Map UniqueRequesterId ChatId)
    -> (c -> Message LobbyChatMsg)
    -> U.OutChan c
    -> IO ()
startLobbyChat ids fn chan = do
    rmsg <- U.readChan chan
    case fn rmsg of
      _ -> pure ()

-- generateChatId :: UniqueModuleId -> UniqueRequesterId -> ChatId
-- generateChatId (UniqueModuleId umid) (UniqueRequesterId urid) =
--     ChatId $ hashWithSalt (hashUnique umid) (hashUnique urid)

data LobbyChatMsg
    = Msg T.Text
    | Whisper ChatId
              T.Text
    | Command Command

newtype ChatId =
    ChatId Int
    deriving (Show,Eq,Ord,Hashable)

data Command
    = Kick ChatId
    | Promote ChatId
    | Ban ChatId
    | Mute ChatId
    | Ignore ChatId
