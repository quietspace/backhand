module Backhand.Room.Types where

import Control.Concurrent.STM
import Data.Aeson
import qualified Data.Text as T

type RoomId = T.Text

-- | Type alias for partially decoded message data structures. For now, this is
-- simply a JSON object.
type MsgData = Object

type RoomMsg = MsgData


-------- Clients --------

type ClientId = Integer

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
