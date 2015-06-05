{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Backhand.Behavior.Chat where

import Control.Monad
import Data.Aeson
import qualified Data.Text as T
import Data.List hiding (union)
import Reactive.Banana
import Reactive.Banana.Frameworks

import Backhand.Room


-- | Data structure representing a chat message.
data ChatMsg
    = ChatMsg T.Text T.Text
    deriving Show

-- | Data structure representing an event such as a user joining or leaving.
data ChatEvent
    = UserJoined T.Text
    | UserParted T.Text

data ChatLogEntry = ChatLogEntryMsg ChatMsg | ChatLogEntryEvent ChatEvent

-- | A response to a chat log request.
data ChatLogMsg = ChatLogMsg [ChatLogEntry]

-- | A message requesting chat logs.
data ChatLogRequest = ChatLogRequest


-- | Room behavior which implements a simple chat room.
chatRoomBehavior :: RoomBehavior
chatRoomBehavior clientMsgEvt clientEvt =
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


  
-------- JSON encoding/decoding stuff --------

instance FromJSON ChatMsg where
    parseJSON (Object v) = do
        msgType <- v .: "type"
        unless ((msgType :: T.Text) == "chat-msg")
               (fail "not a chat message: wrong message type")
        ChatMsg "unknown" <$> v .: "msg"
    parseJSON _ = fail "expected an object"

instance ToJSON ChatMsg where
    toJSON (ChatMsg sender message) = object
        [ "type"   .= String "chat-msg"
        , "sender" .= sender
        , "msg"    .= message ]


instance ToJSON ChatEvent where
    toJSON (UserJoined user) = object
        [ "type" .= String "user-join"
        , "user" .= user ]
    toJSON (UserParted user) = object
        [ "type" .= String "user-part"
        , "user" .= user ]


instance ToJSON ChatLogEntry where
    toJSON (ChatLogEntryMsg msg) = toJSON msg
    toJSON (ChatLogEntryEvent evt) = toJSON evt


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
