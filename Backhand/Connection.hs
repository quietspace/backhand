{-# LANGUAGE OverloadedStrings #-}
-- | This module handles WebSocket connections.
module Backhand.Connection
    ( websockHandler
    ) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Data.Aeson
import qualified Data.Text as T
import Network.WebSockets

import Backhand.Core


-- | Data structure which represents a message sent from the client over the
-- WebSocket.
data ClientWebSockMsg
    = JoinRoom String -- ^ "join" - Join the room with the given ID.
    | PartRoom String -- ^ "part" - Leave the room with the given ID.
    | MsgRoom String Object -- ^ "msg" - Send the given JSON object to the room
                            -- with the given ID. Should respond with an error
                            -- if no such room exists.


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
    doAccept = do
      conn <- acceptRequest pending
      sendDataMessage conn (Text "Hello world!")
      runResourceT $ websockLoop core conn


-- | Main loop for handling websocket connections.
websockLoop :: BackhandCore -> Connection -> ResourceT IO ()
websockLoop core conn = forever doLoop
  where
    doLoop :: ResourceT IO ()
    doLoop = (liftIO $ receiveDataMessage conn) >>= handleDataMsg
    handleDataMsg (Text msgStr) =
      maybe (liftIO $ putStrLn "Failed to decode message.")
            (handleMessage core conn)
            (decode msgStr)
    handleDataMsg msg =
      liftIO $ putStrLn ("Got unknown message: " ++ show msg)


handleMessage :: BackhandCore -> Connection -> ClientWebSockMsg -> ResourceT IO ()
handleMessage core conn (JoinRoom roomId) = do
    hand <- joinRoom core roomId
    liftIO $ putStrLn ("Joined room " ++ roomId)
handleMessage core conn (PartRoom roomId) =
    liftIO $ putStrLn "PartRoom is not implemented."
handleMessage core conn (MsgRoom roomId msg) =
    liftIO $ putStrLn "MsgRoom is not implemented."


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
