{-# LANGUAGE OverloadedStrings #-}
module Backhand.Room.Types where

import Control.Concurrent.STM
import Data.Aeson
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T

newtype RoomId = RoomId { roomIdStr :: T.Text }
    deriving (Eq, Ord, Show)


-- | A message type identifier. This identifies what kind of message a
-- particular message is.
type MsgType = T.Text

-- | A partially decoded message. This consists of a message type ID, which is a
-- string identifying what kind of message this is, and a message data object,
-- which is a JSON object containing message data.
data MsgData = MsgData MsgType Object
               deriving (Show)


-------- Clients --------

newtype ClientId = ClientId Integer
    deriving (Eq, Ord, Show)

-- TODO: Move this stuff to a separate module for dealing with clients.
data Client = Client
    { cId :: ClientId
    , cChan :: TQueue MsgData -- ^ Channel for sending messages to the client.
    }


-- | Data structure which represents a message from a client.
data ClientMsg = ClientMsg
    { clientMsgSender :: Client
    , clientMsgData   :: MsgData
    } deriving (Show)

-- | Data structure which represents a client either joining or leaving a room.
data ClientEvent = ClientMsgEvt ClientMsg
                 | ClientJoinEvt Client
                 | ClientPartEvt Client

instance Eq Client where
    a == b = cId a == cId b

instance Ord Client where
    compare a b = compare (cId a) (cId b)

instance Show Client where
    show c = "Client " ++ show (cId c)


-------- Some JSON encoding/decoding stuff --------

instance ToJSON RoomId where
    toJSON (RoomId str) = String str

instance FromJSON RoomId where
    parseJSON (String str) = return $ RoomId str
    parseJSON _ = fail "expected a string"


instance ToJSON MsgData where
    toJSON (MsgData mtype mdata) =
        Object $ H.insert "type" (String mtype) mdata

instance FromJSON MsgData where
    parseJSON (Object o) = do
        mtype <- o .: "type"
        return $ MsgData mtype $ H.delete "type" o
    parseJSON _ = fail "expected an object"
