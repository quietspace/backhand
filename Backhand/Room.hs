{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Backhand.Room where

import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.Aeson
import qualified Data.Text as T

import Debug.Trace

-- | Creates a new room with the given event handler and initial state.
mkRoom :: forall s. RoomId -> RoomEventHandler s -> s -> STM Room
mkRoom roomId handler initState = do
    clients <- newTVar []
    nextCId <- newTVar 0
    hState <- newTVar initState
    return Room
        { rId = roomId
        , rClients = clients
        , rNextClientId = nextCId
        , rEvtHandler = handler
        , rHandlerState = hState
        }

-------- Event Handler Stuff --------

-- | Data structure which represents an event that the room handler should handle.
data RoomEvent
    = ClientMsg Client RoomMsg -- ^ A message from a client.


-- | Type for a room's event handler. These handlers are responsible for a
-- particular room's behavior. The room handler is a function which takes an
-- event, performs some actions in response to the event, and then returns a new
-- event handler function. This way, state can be kept by passing data to the
-- new handler function without the room having to know about the state.
type RoomEventHandler s = s -> RoomEvent -> RoomM s

-- | Monad for executing actions within the context of a room.
newtype RoomM a = RoomM { unRoomM :: ReaderT Room STM a }
    deriving (Functor, Applicative, Monad)


-- | Sends a message to the given client.
sendMessage :: Client -> RoomMsg -> RoomM ()
sendMessage client msg = RoomM $ lift $ writeTQueue (cChan client) msg


-- | Message handler which just echoes all messages back.
testRoomHandler :: Integer -> RoomEvent -> RoomM Integer
testRoomHandler x (ClientMsg client msg) = trace ("Handle message: " ++ show msg ++ " " ++ show x) $ do
    sendMessage client msg
    return (x + 1)


-------- Room State Stuff --------

type RoomId = T.Text

type RoomMsg = Object

-- | Container object for a room's state. This holds the "internal state" of the
-- room; stuff internal to Backhand. It also holds the room's message handler
-- and the state for that handler.
data Room = forall s. Room
    { rId           :: RoomId
    , rClients      :: TVar [Client]
    , rNextClientId :: TVar ClientId
    , rEvtHandler   :: RoomEventHandler s
    , rHandlerState :: TVar s
    }

-- | Handles the given event with the given room, updating the handler's state.
handleEvent :: RoomEvent -> RoomM ()
handleEvent evt = do
    Room{ rEvtHandler = handler, rHandlerState = stateTV } <- RoomM ask
    oldState <- RoomM $ lift $ readTVar stateTV
    newState <- handler oldState evt
    RoomM $ lift $ writeTVar stateTV newState


type ClientId = Integer

-- TODO: Move this stuff to a separate module for dealing with clients.
data Client = Client
    { cId :: ClientId
    , cChan :: TQueue RoomMsg -- ^ Channel for sending messages to the client.
    }

instance Eq Client where
    a == b = cId a == cId b


              
-- | Represents a "handle" to a particular room. This is given to clients when
-- they register with the room and is associated with a particular client ID
-- within that room. It can be used to send and receive messages as that client.
-- This object *must* be cleaned up when the client disconnects.
data RoomHandle = RoomHandle
    { rhRecvChan   :: TQueue RoomMsg -- ^ Queue for received messages from the room.
    , rhRoom       :: Room           -- ^ The room this handle is connected to.
    , rhClientId   :: ClientId       -- ^ This client's ID within the room.
    , rhReleaseKey :: ReleaseKey     -- ^ ResourceT release key for this room.
    }


-- | Sends a message from a client to the room connected on the given handle.
msgRoom :: (MonadIO m) => RoomHandle -> RoomMsg -> m ()
msgRoom hand msg = liftIO $
    atomically $ runReaderT (unRoomM $ handleEvent $ ClientMsg client msg) (rhRoom hand)
  where
    client = Client (rhClientId hand) (rhRecvChan hand)

-- | STM action which receives a message from the room connected to the given handle.
recvRoomMsgSTM :: RoomHandle -> STM RoomMsg
recvRoomMsgSTM hand = do
    readTQueue $ rhRecvChan hand

-------- Internal Stuff (used by Core) --------
  
-- | True if the given room has no clients connected.
isEmptyRoom :: Room -> STM Bool
isEmptyRoom = fmap null . readTVar . rClients
  
-- | An STM action which joins the given room and returns a @RoomHandle@
-- representing the connection.  The @RoomHandle@ *must* be cleaned up when it
-- is no longer needed. To ensure proper resource handling, this function should
-- not be called directly. Instead, use `Backhand.Core.joinRoom`.
joinRoomSTM :: Room -> STM RoomHandle
joinRoomSTM room = do
    recvChan <- newTQueue
    clientId <- readTVar $ rNextClientId room
    let client = Client
                 { cId = clientId
                 , cChan = recvChan
                 }
    modifyTVar (rClients room) (client:)
    modifyTVar (rNextClientId room) (+1)
    printClientCount room
    return RoomHandle
      { rhRecvChan = recvChan
      , rhClientId = cId client
      , rhRoom = room
      , rhReleaseKey = undefined -- This should be set later by the joinRoom
                                 -- function.
      }

-- | An STM action which closes the given room handle, disconnecting the client
-- from the room. Like @joinRoomSTM@, this function is primarily for internal
-- use. `Backhand.Core.partRoom` should be used instead to ensure cleanup is
-- handled properly.
partRoomSTM :: RoomHandle -> STM ()
partRoomSTM (RoomHandle { rhRoom = room, rhClientId = clientId }) = do
    modifyTVar (rClients room) removeClient
    printClientCount room
  where
    -- Keep only clients with client IDs different from ours.
    removeClient = filter (\c -> cId c /= clientId)


-- | Debugging function.
printClientCount :: Room -> STM ()
printClientCount room = do
    clientCount <- length <$> readTVar (rClients room)
    trace ("Client count: " ++ show clientCount) $ return ()
