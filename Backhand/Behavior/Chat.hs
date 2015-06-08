{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Backhand.Behavior.Chat where

import Prelude hiding (id, (.))

import Control.Applicative -- Implicit in GHC 7.10
import Control.Auto
import Control.Auto.Blip
import Control.Monad
import Data.Aeson
import qualified Data.Text as T

import Backhand.Room
import Backhand.Util.Aeson
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
chatMsg :: (Monad m) => RoomAuto m (Blip ChatMsg)
chatMsg = modifyBlips mkMsg <<< msgsFromJSON
  where
    mkMsg :: (Client, ChatMsg) -> ChatMsg
    mkMsg (client, (ChatMsg _ msg)) = ChatMsg (toClientName client) msg

-- | Blip stream with the names of joining users.
userJoin :: (Monad m) => RoomAuto m (Blip T.Text)
userJoin = modifyBlips toClientName <<< clientJoin

-- | Blip stream with the names of leaving users.
userPart :: (Monad m) => RoomAuto m (Blip T.Text)
userPart = modifyBlips toClientName <<< clientPart

-- | Blips on user joins/parts/messages.
chatEvents :: (Monad m) => RoomAuto m (Blip ChatEvent)
chatEvents =
    -- This merge shouldn't lose any events, since only one of these events can
    -- occur at a time.
       (modifyBlips ChatMsgEvt  <<< chatMsg)
    &> (modifyBlips UserJoinEvt <<< userJoin)
    &> (modifyBlips UserPartEvt <<< userPart)


-- | Auto containing the chat logs.
chatLogs :: (Monad m) => RoomAuto m [ChatEvent]
chatLogs = scanB_ (flip (:)) [] <<< chatEvents


-- | Emits when a client requests chat logs.
chatLogRequest :: forall m. (Monad m) => RoomAuto m (Blip Client)
chatLogRequest = modifyBlips fst <<< requests
  where
    requests :: RoomAuto m (Blip (Client, ChatLogRequest))
    requests = msgsFromJSON


-- | A behavior that sends chat logs to clients when they are requested.
sendChatLogs :: RoomBehavior
sendChatLogs = sendMessages <<< perBlip encodeMessages <<< requestedLogs
  where
    -- | Blips with the contents of the chat log and a client to send
    -- them to whenever they are requested.
    requestedLogs :: Monad m => RoomAuto m (Blip (Client, ChatLogMsg))
    requestedLogs =
        perBlip (second $ arr (ChatLogMsg . reverse))
                    <<< sample chatLogs chatLogRequest


-- | Broadcasts chat events to all clients.
sendChatEvents :: RoomBehavior
sendChatEvents = broadcastMsgs (modifyBlips toJSONObj <<< chatEvents)

  
-------- JSON encoding/decoding stuff --------

instance FromJSON ChatMsg where
    parseJSON (Object v) = do
        msgType <- v .: "type"
        if ((msgType :: T.Text) == "chat-msg")
           then ChatMsg "unknown" <$> v .: "msg"
           else fail "not a chat message: wrong message type"
    parseJSON _ = fail "expected an object"

instance ToJSONObject ChatMsg where
    toJSONObj (ChatMsg sender message) = jsonObj
        [ "type"   .= String "chat-msg"
        , "sender" .= sender
        , "msg"    .= message ]


instance ToJSONObject ChatEvent where
    toJSONObj (ChatMsgEvt msg) = toJSONObj msg
    toJSONObj (UserJoinEvt user) = jsonObj
        [ "type" .= String "user-join"
        , "user" .= user ]
    toJSONObj (UserPartEvt user) = jsonObj
        [ "type" .= String "user-part"
        , "user" .= user ]

instance ToJSON ChatEvent where
    toJSON = Object . toJSONObj


instance ToJSONObject ChatLogMsg where
    toJSONObj (ChatLogMsg messages) = jsonObj
        [ "type" .= String "chat-logs"
        , "msgs" .= messages ]


instance FromJSON ChatLogRequest where
    parseJSON (Object v) = do
        msgType <- v .: "type"
        if (msgType :: T.Text) == "chat-log-request"
           then return ChatLogRequest
           else mzero
    parseJSON _ = fail "expected an object"
