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
    , MsgData
    , sendMessages
    , encodeMessages
    , sendMultiMessages
    , broadcastMsgs
    , msgsFromJSON
    , clientJoin
    , clientPart
    , clientMsg
    ) where

import Prelude hiding ((.))

import Control.Applicative -- Implicit in GHC 7.10
import Control.Auto
import Control.Auto.Blip
import Data.Aeson
import Data.List

import Backhand.Room.Types
import Backhand.Room.Monad
import Backhand.Util.Aeson
import Backhand.Util.Auto


-------- Event Handler Stuff --------

-- | The `RoomBehavior` defines a room's actual behavior in response to
-- messages. It consists of an auto which takes client events as input and
-- outputs a list of messages to send.
type RoomBehavior = Auto' ClientEvent [(Client, MsgData)]

-- | Shorthand for an auto which takes a client event as its input.
type RoomAuto m b = Auto m ClientEvent b


-- | Merges two behaviors into one. All input events are sent to both behaviors
-- and the output messages from both behaviors are combined.
mergeBehaviors :: RoomBehavior -> RoomBehavior -> RoomBehavior
mergeBehaviors a b =
    -- Fanout to both behaviors and combine the output lists.
    (a &&& b) >>> arr (uncurry (++))

-- | Monoid instance for `RoomBehavior` where `mappend` merges behaviors and
-- `mempty` is a behavior which does nothing.
instance Monoid RoomBehavior where
    mappend = mergeBehaviors
    mempty = arr (const [])


-- | Blip stream which outputs when a client joins the room.
clientJoin :: Monad m => RoomAuto m (Blip Client)
clientJoin = emitJusts match
  where
    match (ClientJoinEvt c) = Just c
    match _ = Nothing

-- | Auto which outputs a client when a client leaves the room.
clientPart :: Monad m => RoomAuto m (Blip Client)
clientPart = emitJusts match
  where
    match (ClientPartEvt c) = Just c
    match _ = Nothing

-- | Auto which outputs a client when a client sends a message.
clientMsg :: Monad m => RoomAuto m (Blip ClientMsg)
clientMsg = emitJusts match
  where
    match (ClientMsgEvt c) = Just c
    match _ = Nothing


-- | An auto that keeps track of all the clients in the room.
clientList :: Monad m => RoomAuto m [Client] -- TODO: Implement serialization for this auto
clientList = scanB_ (\a f -> f a) [] <<< updates
  where
    updates = addClient &> delClient
    addClient = modifyBlips (:)    <<< clientJoin
    delClient = modifyBlips delete <<< clientPart


-- | Encodes the second element of a stream of @(Client, b)@ tuples so they can
-- be sent to clients. @b@ must be an instance of `ToJSONObject`.
encodeMessages :: (Monad m, ToJSONObject b) => Auto m (Client, b) (Client, MsgData)
encodeMessages = second (arr toJSONObj)

-- | Takes a blip stream of messages from upstream and outputs them to be sent
-- to clients.
sendMessages :: Monad m => Auto m (Blip (Client, MsgData)) [(Client, MsgData)]
sendMessages =
    -- When there's no input, output an empty list. When there is input, output
    -- a list with the message to send.
    fromBlipsWith [] (:[])

-- | Like `sendMessages`, but can send multiple messages at once.
sendMultiMessages :: (Monad m) => Auto m (Blip [(Client, MsgData)]) [(Client, MsgData)]
sendMultiMessages =
    -- When there's no input, output an empty list. When there is input, output
    -- it.
    fromBlips []


-- | Creates a room behavior which broadcasts the output of the given auto to
-- all clients.
broadcastMsgs :: forall m. (Monad m) =>
                 RoomAuto m (Blip MsgData)
              -> RoomAuto m [(Client, MsgData)]
broadcastMsgs input = sendMultiMessages <<< combined
  where
    combined = modifyBlips toAll <<< clientListBlips
    -- Blip stream that outputs a tuple with a list of all the clients and the
    -- upstream blip value whenever the `input` stream blips.
    clientListBlips :: RoomAuto m (Blip (MsgData, [Client]))
    clientListBlips = sample clientList input
    -- Take a list of clients and a message and build a list of tuples to send
    -- the message to the clients.
    toAll :: (MsgData, [Client]) -> [(Client, MsgData)]
    toAll (msg, clients) = map (, msg) clients


-- | Decodes incoming messages from JSON.
msgsFromJSON :: forall m b. (Monad m, FromJSON b) =>
                RoomAuto m (Blip (Client, b))
msgsFromJSON = mapMaybeB process <<< clientMsg
  where
    process :: ClientMsg -> Maybe (Client, b)
    process (ClientMsg client msg) =
        let decoded = fromJSON $ Object msg
        in case decoded of
             Success val -> Just (client, val)
             Error _ -> Nothing
