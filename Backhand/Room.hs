{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Backhand.Room where

import Control.Applicative -- Implicit in GHC 7.10
import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.Aeson
import qualified Data.Text as T
import Reactive.Banana
import Reactive.Banana.Frameworks

import System.IO.Unsafe

import Debug.Trace

-- | Creates a new room with the given event handler and initial state.
mkRoom :: RoomId -> RoomBehavior -> STM Room
mkRoom roomId behavior = do
  clients <- newTVar []
  nextCId <- newTVar 0
  -- Unsafe IO is necessary here to keep this function as an STM
  -- action. Hopefully won't cause problems.
  (handleMsg, handleClientsChange) <- return $ unsafePerformIO $ do
    (addMsgEvt, handleMsg) <- newAddHandler
    (addClientListEvt, handleClientsChange) <- newAddHandler
    network <- behavior addMsgEvt addClientListEvt
    actuate network
    return (handleMsg, handleClientsChange)
  return Room
         { rId = roomId
         , rClients = clients
         , rNextClientId = nextCId
         , rHandleMsg = handleMsg
         , rHandleClients = handleClientsChange
         }


-------- Event Handler Stuff --------

-- | Data structure which represents a message from a client.
data ClientMsg = ClientMsg
    { clientMsgSender :: Client
    , clientMsgData   :: MsgData
    } deriving (Show)


-- | The `RoomBehavior` defines a room's actual behavior in response to
-- messages. It consists of a function which takes a set of events and behaviors
-- as arguments and compiles an event network which should implement the desired
-- behavior.
type RoomBehavior = AddHandler ClientMsg -> AddHandler [Client] -> IO EventNetwork
-- TODO: Get rid of the IO monad here.



-- | Sends a message to the given client.
sendMessage :: Client -> MsgData -> IO ()
sendMessage client msg = atomically $ writeTQueue (cChan client) msg

-- | Takes `(Client, MsgData)` tuples from the given input event and sends each
-- message to the appropriate client.
sendMessages :: (Frameworks t) => Event t (Client, MsgData) -> Moment t ()
sendMessages msgs = reactimate (uncurry sendMessage <$> msgs)


-- | This function is `sendMessages`, but it accepts lists of @(Client,
-- MsgData)@ tuples and sends messages to all of them.
sendMultiMessages :: (Frameworks t) => Event t [(Client, MsgData)] -> Moment t ()
sendMultiMessages eMsgList = reactimate (mapM_ (uncurry sendMessage) <$> eMsgList)


-- | Takes an event stream of client messages and attempts to read each
-- message's data as JSON using some `FromJSON` instance. Successfully decoded
-- objects are sent down stream tupled together with the client that sent them.
eventsFromJSON :: forall t. forall a. FromJSON a =>
                  Event t ClientMsg -> Event t (Client, a)
eventsFromJSON evt = filterJust (process <$> evt)
  where
    process :: ClientMsg -> Maybe (Client, a)
    process (ClientMsg client msg) =
        let decoded = fromJSON $ Object msg
        in case decoded of
             Success val -> Just (client, val)
             Error err -> Nothing


-------- Room State Stuff --------

type RoomId = T.Text

-- | Type alias for partially decoded message data structures. For now, this is
-- simply a JSON object.
type MsgData = Object

type RoomMsg = MsgData


-- | Container object for a room's state. This holds the "internal state" of the
-- room; stuff internal to Backhand. It also holds the room's message handler
-- and the state for that handler.
data Room = Room
    { rId            :: RoomId
    , rClients       :: TVar [Client]
    , rNextClientId  :: TVar ClientId
    , rHandleMsg     :: ClientMsg -> IO () -- ^ IO action to fire the client
                                           -- message event.
    , rHandleClients :: [Client] -> IO () -- ^ IO action to let the behavior
                                          -- know the client list changed.
    }

-- | Handles the given event with the given room, updating the handler's state.
-- handleEvent :: RoomEvent -> RoomM ()
-- handleEvent evt = do
--     Room{ rEvtHandler = handler, rHandlerState = stateTV } <- RoomM ask
--     oldState <- RoomM $ lift $ readTVar stateTV
--     newState <- handler oldState evt
--     RoomM $ lift $ writeTVar stateTV newState


type ClientId = Integer

-- TODO: Move this stuff to a separate module for dealing with clients.
data Client = Client
    { cId :: ClientId
    , cChan :: TQueue RoomMsg -- ^ Channel for sending messages to the client.
    }

instance Eq Client where
    a == b = cId a == cId b

instance Show Client where
    show c = "Client " ++ show (cId c)

              
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
msgRoom :: (MonadIO m) => RoomHandle -> MsgData -> m ()
msgRoom hand msg =
    -- To handle a message, all we need to do is call the room's `rHandleMsg`
    -- function and the FRP stuff should take over from there.
    liftIO $ rHandleMsg (rhRoom hand) (ClientMsg client msg)
  where
    client = Client (rhClientId hand) (rhRecvChan hand)
    -- atomically $ runReaderT (unRoomM $ handleEvent $ ClientMsg client msg) (rhRoom hand)

-- | STM action which receives a message from the room connected to the given handle.
recvRoomMsgSTM :: RoomHandle -> STM RoomMsg
recvRoomMsgSTM hand = do
    readTQueue $ rhRecvChan hand

-------- Internal Stuff (used by Core) --------

-- | Sends the room's behavior a copy of the room's current client list. This
-- should be called after any call to `joinRoomSTM` or `partRoomSTM`.
updateClientList :: Room -> IO ()
updateClientList room = do
    cs <- readTVarIO (rClients room)
    rHandleClients room cs
  
-- | True if the given room has no clients connected.
isEmptyRoom :: Room -> STM Bool
isEmptyRoom = fmap null . readTVar . rClients
  
-- | An STM action which joins the given room and returns a @RoomHandle@
-- representing the connection.  The @RoomHandle@ *must* be cleaned up when it
-- is no longer needed. Furthermore, after this function is called, it is
-- necessary to call the `updateClientList` function to ensure the room's
-- behavior knows about the new client. Therefore, to ensure proper resource
-- handling, this function should not be called directly. Instead, use
-- `Backhand.Core.joinRoom`.
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
