module Backhand.Room.Types where

import Control.Concurrent.STM
import Data.Aeson
import qualified Data.Text as T

newtype RoomId = RoomId { roomIdStr :: T.Text }
    deriving (Eq, Ord, Show)

-- | Type alias for partially decoded message data structures. For now, this is
-- simply a JSON object.
type MsgData = Object

type RoomMsg = MsgData


-------- Clients --------

newtype ClientId = ClientId Integer
    deriving (Eq, Ord, Show)

-- TODO: Move this stuff to a separate module for dealing with clients.
data Client = Client
    { cId :: ClientId
    , cChan :: TQueue RoomMsg -- ^ Channel for sending messages to the client.
    }


-- | Data structure which represents a message from a client.
data ClientMsg = ClientMsg
    { clientMsgSender :: Client
    , clientMsgData   :: MsgData
    } deriving (Show)


instance Eq Client where
    a == b = cId a == cId b

instance Show Client where
    show c = "Client " ++ show (cId c)


-------- Some JSON encoding/decoding stuff --------

instance ToJSON RoomId where
    toJSON (RoomId str) = String str

instance FromJSON RoomId where
    parseJSON (String str) = return $ RoomId str
    parseJSON _ = fail "expected a string"
