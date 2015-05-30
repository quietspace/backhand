{-# LANGUAGE OverloadedStrings #-}
-- | This module handles WebSocket connections.
module Backhand.Connection
    ( websockHandler
    ) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Resource
import Data.Aeson
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Monoid
import qualified Data.Text as T
import Network.WebSockets

import Backhand.Core
import Backhand.Room


-- | Internal state object for WebSocket threads.
data WebSockState = WebSockState
    { wssRoomHands :: Map RoomId RoomHandle
    , wssCore :: BackhandCore
    , wssConn :: Connection
    }

-- | Convenience function for modifying the room handles list. Maybe this should
-- be replaced with lenses.
updateRoomHands :: (Map RoomId RoomHandle -> Map RoomId RoomHandle)
                -> WebSockM ()
updateRoomHands func =
    modify (\s -> s { wssRoomHands = func $ wssRoomHands s })

initSockState :: BackhandCore -> Connection -> WebSockState
initSockState = WebSockState M.empty

-- | Crazy monad stack for WebSocket stuff.
type WebSockM = ResourceT (StateT WebSockState IO)


-- | Data structure which represents a message sent from the client over the
-- WebSocket.
data ClientWebSockMsg
    = JoinRoom RoomId -- ^ "join" - Join the room with the given ID.
    | PartRoom RoomId -- ^ "part" - Leave the room with the given ID.
    | MsgRoom RoomId Object -- ^ "msg" - Send the given JSON object to the room
                            -- with the given ID. Should respond with an error
                            -- if no such room exists.

-- | Data structure which represents a message sent from the server over the
-- WebSocket.
data ServerWebSockMsg
    = JoinedRoom RoomId -- ^ "joined" - Successfully joined the given room.
    | PartedRoom RoomId -- ^ "parted" - Successfully left the given room.
    | MsgFromRoom RoomId Object -- ^ "msg" - Received the given message from the
                                -- given room.
    | WebSockError T.Text -- ^ Indicates some sort of error occurred.


-- | A WebSocket `ServerApp` which handles incoming connections and connects
-- them to the given core.
websockHandler :: BackhandCore -> PendingConnection -> IO ()
websockHandler core pending = do
    -- First, we check the request's subprotocols. We only support backhand-json
    -- right now, but other protocols may be supported in the future.
    let protocols = getRequestSubprotocols $ pendingRequest pending
    if "backhand-json" `elem` protocols
       then doAccept
       else rejectRequest pending "Only the \"backhand-json\" subprotocol is supported right now."
  where
    doAccept :: IO ()
    doAccept = do
      conn <- acceptRequest pending
      evalStateT (runResourceT websockLoop) (initSockState core conn)


-- | Main loop for handling websocket connections.
websockLoop :: WebSockM ()
websockLoop = forever doLoop
  where
    doLoop :: WebSockM ()
    doLoop = recvMessage >>= handleMessage


handleMessage :: ClientWebSockMsg -> WebSockM ()
handleMessage (JoinRoom roomId) = do
    rooms <- gets wssRoomHands
    if roomId `M.member` rooms
       then sendError ("Failed to join room. Already in room " <> roomId)
       else do
         core <- gets wssCore
         hand <- joinRoom core roomId
         updateRoomHands $ M.insert roomId hand
         sendMessage $ JoinedRoom roomId
         liftIO $ putStrLn ("Joined room " ++ show roomId)
handleMessage (PartRoom roomId) = do
    core <- gets wssCore
    roomHand <- gets ((M.! roomId) . wssRoomHands)
    updateRoomHands $ M.delete roomId
    partRoom core roomHand
    sendMessage $ PartedRoom roomId
    liftIO $ putStrLn ("Parted from room " ++ show roomId)
handleMessage (MsgRoom _ _) =
    liftIO $ putStrLn "MsgRoom is not implemented."


-- | Sends a `ServerWebSockMsg` to the connected client.
sendMessage :: ServerWebSockMsg -> WebSockM ()
sendMessage msg = do
    conn <- gets wssConn
    liftIO $ sendTextData conn $ encode msg

sendError :: T.Text -> WebSockM ()
sendError = sendMessage . WebSockError

-- | Receives a `ClientWebSockMsg` from the connected client. If the message
-- cannot be decoded, sends an error to the client and waits for another
-- message. This process will repeat until a valid message is received.
recvMessage :: WebSockM ClientWebSockMsg
recvMessage = do
    conn <- gets wssConn
    msgM <- decode <$> liftIO (receiveData conn)
    maybe (sendError "Failed to decode message." >> recvMessage) return msgM


-------- Some JSON encoding/decoding stuff --------

instance FromJSON ClientWebSockMsg where
    parseJSON (Object v) = do
        msgType <- (v .: "type")
        case msgType :: T.Text of
          "join" -> JoinRoom <$> v .: "room"
          "part" -> PartRoom <$> v .: "room"
          "msg"  -> MsgRoom  <$> v .: "room"
                             <*> v .: "msg"
          _ -> mzero
    parseJSON _ = mzero


instance ToJSON ServerWebSockMsg where
    toJSON (JoinedRoom roomId) = object
        [ "type" .= String "joined"
        , "room" .= roomId ]
    toJSON (PartedRoom roomId) = object
        [ "type" .= String "parted"
        , "room" .= roomId ]
    toJSON (MsgFromRoom roomId msg) = object
        [ "type" .= String "msg"
        , "room" .= roomId
        , "msg" .= msg ]
    toJSON (WebSockError msg) = object
        [ "type" .= String "error"
        , "msg" .= msg ]
