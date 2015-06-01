{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Backhand.Behavior.Chat where

import Control.Monad
import Data.Aeson
import qualified Data.Text as T
import Reactive.Banana
import Reactive.Banana.Frameworks

import Backhand.Room


-- | Data structure representing a chat message.
data ChatMsg = ChatMsg T.Text T.Text deriving Show


-- | A response to a chat log request.
data ChatLogMsg = ChatLogMsg [ChatMsg]

-- | A message requesting chat logs.
data ChatLogRequest = ChatLogRequest


-- | Room behavior which implements a simple chat room.
chatRoomBehavior :: RoomBehavior
chatRoomBehavior addClientMsg addClientList =
  let eventNet :: forall t. (Frameworks t) => Moment t ()
      eventNet = do
        eClientMsg <- fromAddHandler addClientMsg
        bClientList <- fromChanges [] addClientList

        let eChatMsg :: Event t ChatMsg
            eChatMsg = evtChatMsg eClientMsg

        -- TODO: Clear old log entries when they get too long.
        let bChatLogs :: Behavior t [ChatMsg]
            bChatLogs = accumB [] ((:) <$> eChatMsg)

        sendChatMsgs bClientList eChatMsg
        sendChatLogs bChatLogs eClientMsg
  in compile eventNet


-- | Takes an event stream of client messages and outputs chat messages.
evtChatMsg :: Event t ClientMsg -> Event t ChatMsg
evtChatMsg eClientMsg =
    processMsgData <$> eventsFromJSON eClientMsg
  where
    processMsgData (client, (ChatMsg _ msg)) =
        ChatMsg (T.pack $ show $ cId client) msg


-- | Takes a chat message event stream and notifies all the clients in the given
-- behavior when a message is posted.
sendChatMsgs :: forall t. Frameworks t =>
                Behavior t [Client] -> Event t ChatMsg -> Moment t ()
sendChatMsgs bClientList eChatMsg = sendMultiMessages eSendMsg
  where
    eChatMsgData :: Event t MsgData
    eChatMsgData = (\(Object o) -> o) <$> toJSON <$> eChatMsg
    eSendMsg :: Event t [(Client, MsgData)]
    eSendMsg = apply bBroadcast (repeat <$> eChatMsgData)
    bBroadcast :: Behavior t ([MsgData] -> [(Client, MsgData)])
    bBroadcast = zip <$> bClientList

    
-- | Takes a behavior containing a list of chat logs and the client command
-- event stream and sends clients chat logs whenever they request them.
sendChatLogs :: forall t. Frameworks t =>
                Behavior t [ChatMsg] -> Event t ClientMsg -> Moment t ()
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
