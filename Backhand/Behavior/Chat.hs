{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Backhand.Behavior.Chat where

import Prelude hiding ((.))

import Control.Monad
import Control.Wire
import Data.Aeson
import qualified Data.Text as T

import Backhand.Room


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
chatRoomBehavior =
    const () <$> (sendChatLogs &&& sendChatEvents)


-- | Fires when a chat message is received.
chatMsg :: (MonadRoom m) => RoomWire s e m (Event ChatMsg)
chatMsg = fmap mkMsg <$> msgsFromJSON
  where
    mkMsg :: (Client, ChatMsg) -> ChatMsg
    mkMsg (client, (ChatMsg _ msg)) = ChatMsg (toClientName client) msg

-- | Event which emits the names of joining users.
userJoin :: (MonadRoom m) => RoomWire s e m (Event T.Text)
userJoin = fmap toClientName <$> clientJoin

-- | Event which emits the names of leaving users.
userPart :: (MonadRoom m) => RoomWire s e m (Event T.Text)
userPart = fmap toClientName <$> clientPart

-- | Fires on user joins/parts.
chatEvents :: (MonadRoom m) => RoomWire s e m (Event ChatEvent)
chatEvents =
    -- This merge shouldn't lose any events, since only one of these events can
    -- occur at a time.
       (fmap ChatMsgEvt  <$> chatMsg)
    &> (fmap UserJoinEvt <$> userJoin)
    &> (fmap UserPartEvt <$> userPart)


-- | Wire containing the chat logs.
chatLogs :: (MonadRoom m, Monoid e) => RoomWire s e m [ChatEvent]
chatLogs = hold . accumE (flip (:)) [] . chatEvents


-- | Emits when a client requests chat logs.
chatLogRequest :: forall s e m. (MonadRoom m) => RoomWire s e m (Event Client)
chatLogRequest = fmap fst <$> (msgsFromJSON :: RoomWire s e m (Event (Client, ChatLogRequest)))


-- | Sends chat logs to clients when they are requested.
sendChatLogs :: forall s e m. (MonadRoom m, Monoid e) => RoomWire s e m ()
sendChatLogs = encodeSendMessages . requestedLogs
  where
    -- | Fires an event with the contents of the chat log and a client to send
    -- them to whenever they are requested.
    requestedLogs :: RoomWire s e m (Event (Client, ChatLogMsg))
    requestedLogs = applyE (tagLogs <$> chatLogs) chatLogRequest
    tagLogs :: [ChatEvent] -> Client -> (Client, ChatLogMsg)
    tagLogs logs client = (client, ChatLogMsg $ reverse logs)


-- | Broadcasts chat events to all clients.
sendChatEvents :: (MonadRoom m, Monoid e) => RoomWire s e m ()
sendChatEvents = broadcastMsgs (fmap toJsonObj <$> chatEvents)


{-
  let eventNet :: forall t. (Frameworks t) => Moment t ()
      eventNet = do
        eClientMsg <- fromAddHandler clientMsgEvt
        eClientEvt <- fromAddHandler clientEvt

        let eClientJoin = evtClientJoin eClientEvt
            eClientPart = evtClientPart eClientEvt
            eChatEvt = (UserJoined <$> T.pack <$> show <$> cId <$> eClientJoin) `union`
                       (UserParted <$> T.pack <$> show <$> cId <$> eClientPart)

        let bClientList :: Behavior t [Client]
            bClientList = accumB [] (((:)    <$> eClientJoin) `union`
                                     (delete <$> eClientPart))

        let eChatMsg :: Event t ChatMsg
            eChatMsg = evtChatMsg eClientMsg

        let eChatLogEvt = (ChatLogEntryMsg   <$> eChatMsg) `union`
                          (ChatLogEntryEvent <$> eChatEvt)

        -- TODO: Clear old log entries when they get too long.
        let bChatLogs :: Behavior t [ChatLogEntry]
            bChatLogs = accumB [] ((:) <$> eChatLogEvt)

        sendChatEntries bClientList eChatLogEvt
        sendChatLogs bChatLogs eClientMsg
  in compile eventNet


-- | Takes an event stream of client messages and outputs chat messages.
evtChatMsg :: Event t ClientMsg -> Event t ChatMsg
evtChatMsg eClientMsg =
    processMsgData <$> eventsFromJSON eClientMsg
  where
    processMsgData (client, (ChatMsg _ msg)) =
        ChatMsg (T.pack $ show $ cId client) msg


-- | Takes a log entry event stream and notifies all the clients in the given
-- behavior when a message is posted.
sendChatEntries :: forall t. Frameworks t =>
                   Behavior t [Client] -> Event t ChatLogEntry -> Moment t ()
sendChatEntries bClientList eChatEntry = sendMultiMessages eSendMsg
  where
    eChatMsgData :: Event t MsgData
    eChatMsgData = (\(Object o) -> o) <$> toJSON <$> eChatEntry
    eSendMsg :: Event t [(Client, MsgData)]
    eSendMsg = apply bBroadcast (repeat <$> eChatMsgData)
    bBroadcast :: Behavior t ([MsgData] -> [(Client, MsgData)])
    bBroadcast = zip <$> bClientList

    
-- | Takes a behavior containing a list of chat logs and the client command
-- event stream and sends clients chat logs whenever they request them.
sendChatLogs :: forall t. Frameworks t =>
                Behavior t [ChatLogEntry] -> Event t ClientMsg -> Moment t ()
sendChatLogs bChatLogs eClientMsg = sendMessages eChatLogResponse
  where
    eChatLogRequest :: Event t Client
    eChatLogRequest = fst <$> (eventsFromJSON eClientMsg :: Event t (Client, ChatLogRequest))
    eChatLogResponse =
        (\(logs, client) -> (client, toJSONObj $ ChatLogMsg $ reverse logs))
        <$> apply ((,) <$> bChatLogs) eChatLogRequest
    toJSONObj = (\(Object o) -> o) . toJSON

-}
  
-------- JSON encoding/decoding stuff --------

instance FromJSON ChatMsg where
    parseJSON (Object v) = do
        msgType <- v .: "type"
        if ((msgType :: T.Text) == "chat-msg")
           then ChatMsg "unknown" <$> v .: "msg"
           else fail "not a chat message: wrong message type"
    parseJSON _ = fail "expected an object"

instance ToJSON ChatMsg where
    toJSON (ChatMsg sender message) = object
        [ "type"   .= String "chat-msg"
        , "sender" .= sender
        , "msg"    .= message ]


instance ToJSON ChatEvent where
    toJSON (ChatMsgEvt msg) = toJSON msg
    toJSON (UserJoinEvt user) = object
        [ "type" .= String "user-join"
        , "user" .= user ]
    toJSON (UserPartEvt user) = object
        [ "type" .= String "user-part"
        , "user" .= user ]


instance ToJSON ChatLogMsg where
    toJSON (ChatLogMsg messages) = object
        [ "type" .= String "chat-logs"
        , "msgs" .= messages ]


instance FromJSON ChatLogRequest where
    parseJSON (Object v) = do
        msgType <- v .: "type"
        if (msgType :: T.Text) == "chat-log-request"
           then return ChatLogRequest
           else mzero
    parseJSON _ = fail "expected an object"
