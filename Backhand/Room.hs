{-# LANGUAGE Arrows #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Backhand.Room
    ( RoomBehavior
    , Client (cId)
    , RoomM (..)
    , MonadRoom (..)
    , RoomAuto
    -- * Messages
    , ClientMsg (..)
    , ClientEvent (..)
    , encodeMessages
    , decodeMessages
    , decodeMsgsWith
    , sendMessages
    , sendMultiMessages
    , broadcastMsgs
    , decodedMsgs
    , clientJoin
    , clientPart
    , clientMsg
    , clientList
    , EncodeMsg (..)
    , DecodeMsg (..)
    , MsgData (..)
    , msgData
    ) where

import Control.Auto
import Control.Auto.Blip
import Data.Aeson.Types
import qualified Data.HashMap.Strict as H
import Data.List

import Prelude hiding ((.), id)

import Backhand.Room.Types
import Backhand.Room.Monad
import Backhand.Util.Auto


-------- Event Handler Stuff --------

-- | The `RoomBehavior` defines a room's actual behavior in response to
-- messages. It consists of an auto which takes client events as input and
-- outputs a list of messages to send.
type RoomBehavior = Auto' ClientEvent [(Client, MsgData)]

-- | Shorthand for an auto which takes a client event as its input.
type RoomAuto b = Auto' ClientEvent b


-- | Blip stream which outputs when a client joins the room.
clientJoin :: RoomAuto (Blip Client)
clientJoin = emitJusts match
  where
    match (ClientJoinEvt c) = Just c
    match _ = Nothing

-- | Auto which outputs a client when a client leaves the room.
clientPart :: RoomAuto (Blip Client)
clientPart = emitJusts match
  where
    match (ClientPartEvt c) = Just c
    match _ = Nothing

-- | Auto which outputs a client when a client sends a message.
clientMsg :: RoomAuto (Blip ClientMsg)
clientMsg = emitJusts match
  where
    match (ClientMsgEvt c) = Just c
    match _ = Nothing


-- | An auto that keeps track of all the clients in the room.
clientList :: RoomAuto [Client] -- TODO: Implement serialization for this auto
clientList = scanB_ (\a f -> f a) [] <<< updates
  where
    updates = addClient &> delClient
    addClient = modifyBlips (:)    <<< clientJoin
    delClient = modifyBlips delete <<< clientPart


-- | Encodes the second element of a stream of @(Client, b)@ tuples so they can
-- be sent to clients. @b@ must be an instance of `ToJSONObject`.
--
-- Note: `sendMessages` and friends will all accept any type which is an
-- instance of `EncodeMsg` as input, so it is usually unnecessary to use this
-- function.
encodeMessages :: (Monad m, EncodeMsg b) => Auto m b MsgData
encodeMessages = arr encodeMsg

-- | Decodes messages from JSON.
decodeMessages :: forall m b. (Monad m, DecodeMsg b) => Auto m MsgData (Blip b)
decodeMessages = decodeMsgsWith parseMsg

-- | Decodes messages from JSON with the given parsing function.
decodeMsgsWith :: forall m b. (Monad m) =>
                  (MsgData -> Parser b)
               -> Auto m MsgData (Blip b)
decodeMsgsWith pfunc = emitJusts process
  where
    process :: MsgData -> Maybe b
    process msg = parseMaybe pfunc msg



-- | Takes a blip stream of messages from upstream and outputs them to be sent
-- to clients.
sendMessages :: (EncodeMsg a, Monad m) =>
                Auto m (Blip (Client, a)) [(Client, MsgData)]
sendMessages =
    -- When there's no input, output an empty list. When there is input, output
    -- a list with the message to send.
    fromBlipsWith [] (:[]) <<< perBlip (second encodeMessages)

-- | Like `sendMessages`, but can send multiple messages at once.
sendMultiMessages :: forall a m. (EncodeMsg a, Monad m) =>
                     Auto m (Blip [(Client, a)]) [(Client, MsgData)]
sendMultiMessages =
    -- When there's no input, output an empty list. When there is input, output
    -- it.
    fromBlips [] <<< perBlip (arr (map doEncode))
  where
    doEncode :: (Client, a) -> (Client, MsgData)
    doEncode (c, m) = (c, encodeMsg m)


-- | Creates a room behavior which broadcasts the output of the given auto to
-- all clients.
broadcastMsgs :: forall a. (EncodeMsg a) => RoomAuto (Blip a) -> RoomBehavior
broadcastMsgs input = sendMultiMessages <<< combined
  where
    combined = modifyBlips toAll <<< clientListBlips
    -- Blip stream that outputs a tuple with a list of all the clients and the
    -- upstream blip value whenever the `input` stream blips.
    clientListBlips :: RoomAuto (Blip (a, [Client]))
    clientListBlips = sample clientList input
    -- Take a list of clients and a message and build a list of tuples to send
    -- the message to the clients.
    toAll :: (a, [Client]) -> [(Client, a)]
    toAll (msg, clients) = map (, msg) clients


-- | An auto which outputs a blip stream of decoded client messages.
decodedMsgs :: forall b. (DecodeMsg b) =>
                RoomAuto (Blip (Client, b))
decodedMsgs = clientMsg >>> perBlip decodeMsgs >>> joinB
  where
    decodeMsgs = proc (ClientMsg client msg) -> do
        decoded <- asMaybes <<< decodeMessages -< msg
        onJusts -< case decoded of
                     Just val -> Just (client, val)
                     _ -> Nothing


-------- Message encoding/decoding stuff --------

-- | Class for data structures which can be sent to clients as messages from a
-- room.
class EncodeMsg a where
    encodeMsg :: a -> MsgData

-- | Takes a message type ID and a list of aeson pairs and produces a `MsgData`
-- object.
msgData :: MsgType -> [Pair] -> MsgData
msgData mt ps = MsgData mt (H.fromList ps)

instance EncodeMsg MsgData where
    encodeMsg = id


-- | Class for data structures which can be received from clients as messages to
-- a room.
class DecodeMsg a where
    parseMsg :: MsgData -> Parser a

instance DecodeMsg MsgData where
    parseMsg = return
