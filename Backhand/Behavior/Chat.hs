{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Backhand.Behavior.Chat where

import Control.Applicative -- Implicit in GHC 7.10
import Control.Auto
import Control.Auto.Blip
import Control.Monad
import Data.Aeson
import qualified Data.Text as T

import Prelude hiding (id, (.))

import Backhand.Room
import Backhand.Util.Auto


-- | Data structure representing a chat message.
data ChatMsg
    = ChatMsg T.Text T.Text
    deriving Show

-- | Data structure representing an event such as a user joining or leaving.
data ChatEvent
    = ChatMsgEvt ChatMsg
    | UserJoinEvt T.Text
    | UserPartEvt T.Text

-- | A response to a chat log request.
data ChatLogMsg = ChatLogMsg [ChatEvent]

-- | A message requesting chat logs.
data ChatLogRequest = ChatLogRequest


toClientName :: Client -> T.Text
toClientName = T.pack . show . cId


-- | Room behavior which implements a simple chat room.
chatRoomBehavior :: RoomBehavior
chatRoomBehavior = sendChatLogs
                <> sendChatEvents


-- | Blips when a chat message is received.
chatMsg :: RoomAuto (Blip ChatMsg)
chatMsg = modifyBlips mkMsg <<< decodedMsgs
  where
    mkMsg :: (Client, ChatMsg) -> ChatMsg
    mkMsg (client, (ChatMsg _ msg)) = ChatMsg (toClientName client) msg

-- | Blip stream with the names of joining users.
userJoin :: RoomAuto (Blip T.Text)
userJoin = modifyBlips toClientName <<< clientJoin

-- | Blip stream with the names of leaving users.
userPart :: RoomAuto (Blip T.Text)
userPart = modifyBlips toClientName <<< clientPart

-- | Blips on user joins/parts/messages.
chatEvents :: RoomAuto (Blip ChatEvent)
chatEvents =
    -- This merge shouldn't lose any events, since only one of these events can
    -- occur at a time.
       (modifyBlips ChatMsgEvt  <<< chatMsg)
    &> (modifyBlips UserJoinEvt <<< userJoin)
    &> (modifyBlips UserPartEvt <<< userPart)


-- | Auto containing the chat logs.
chatLogs :: RoomAuto [ChatEvent]
chatLogs = scanB_ (flip (:)) [] <<< chatEvents


-- | Emits when a client requests chat logs.
chatLogRequest :: RoomAuto (Blip Client)
chatLogRequest = modifyBlips fst <<< requests
  where
    requests :: RoomAuto (Blip (Client, ChatLogRequest))
    requests = decodedMsgs


-- | A behavior that sends chat logs to clients when they are requested.
sendChatLogs :: RoomBehavior
sendChatLogs = sendMessages <<< requestedLogs
  where
    -- | Blips with the contents of the chat log and a client to send
    -- them to whenever they are requested.
    requestedLogs :: RoomAuto (Blip (Client, ChatLogMsg))
    requestedLogs =
        perBlip (second $ arr (ChatLogMsg . reverse))
                    <<< sample chatLogs chatLogRequest


-- | Broadcasts chat events to all clients.
sendChatEvents :: RoomBehavior
sendChatEvents = broadcastMsgs chatEvents

  
-------- JSON encoding/decoding stuff --------

instance DecodeMsg ChatMsg where
    parseMsg (MsgData "chat-msg" v) =
        ChatMsg "unknown" <$> v .: "msg"
    parseMsg _ = mzero

instance EncodeMsg ChatMsg where
    encodeMsg (ChatMsg sender message) = msgData "chat-msg"
        [ "sender" .= sender
        , "msg"    .= message ]


instance EncodeMsg ChatEvent where
    encodeMsg (ChatMsgEvt msg) = encodeMsg msg
    encodeMsg (UserJoinEvt user) =
        msgData "user-part" [ "user" .= user ]
    encodeMsg (UserPartEvt user) =
        msgData "user-part" [ "user" .= user ]

instance ToJSON ChatEvent where
    toJSON = toJSON . encodeMsg


instance EncodeMsg ChatLogMsg where
    encodeMsg (ChatLogMsg messages) =
        msgData "chat-logs" [ "msgs" .= messages ]


instance DecodeMsg ChatLogRequest where
    parseMsg (MsgData "chat-log-request" _) =
        return ChatLogRequest
    parseMsg _ = mzero
