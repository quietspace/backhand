{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Backhand.Room
    ( RoomBehavior
    , Client (cId)
    , RoomM (..)
    , MonadRoom (..)
    , RoomWire
    -- * Messages
    , ClientMsg (..)
    , ClientEvent (..)
    , MsgData
    , sendMessages
    , encodeSendMessages
    , sendMultiMessages
    , broadcastMsgs
    , msgsFromJSON
    , clientJoin
    , clientPart
    , clientMsg
    -- * Misc
    , filterMatch
    , applyE
    , toJsonObj
    ) where

import Prelude hiding ((.))

import Control.Applicative -- Implicit in GHC 7.10
import Control.Wire
import Control.Wire.Unsafe.Event
import Data.Aeson
import Data.List

import Backhand.Room.Types
import Backhand.Room.Monad

-------- Event Handler Stuff --------

-- | The `RoomBehavior` defines a room's actual behavior in response to
-- messages. It consists of a wire which takes client events as input and runs in
-- the `RoomM` monad.
type RoomBehavior = RoomWire () () RoomM ()

-- | Shorthand for a wire which takes a client event as its input.
type RoomWire s e m b = Wire s e m (Event ClientEvent) b


filterMatch :: (a -> Maybe b) -> Wire s e m (Event a) (Event b)
filterMatch match =
    mkSF_ $ \mev ->
        case fmap match mev of
          Event (Just x) -> Event x
          _ -> NoEvent


-- | Like @apply@ in reactive banana. Merges the two given wires by applying the
-- function from the second wire's output to the contents of the event in the
-- first wire's output and producing a new wire which outputs the result in an
-- event stream.
applyE :: forall s e m a c b. (Monad m) =>
          Wire s e m a (c -> b)
       -> Wire s e m a (Event c)
       -> Wire s e m a (Event b)
applyE funcWire evtWire = arr mergeOutput <<< unmerged
  where
    unmerged :: Wire s e m a (Event c, c -> b)
    unmerged = evtWire &&& funcWire
    mergeOutput :: (Event c, c -> b) -> Event b
    mergeOutput (Event c, func) = Event $ func c
    mergeOutput _ = NoEvent


-- clientEvent :: (MonadRoom m) => RoomWire s e m (Event ClientEvent)
-- clientEvent = mkGen_ $ \_ -> Right <$> Event <$> getCurrentEvent

-- | Wire which outputs when a client joins the room.
clientJoin :: (MonadRoom m) => RoomWire s e m (Event Client)
clientJoin = filterMatch match
  where
    match (ClientJoinEvt c) = Just c
    match _ = Nothing

-- | Wire which outputs a client when a client leaves the room.
clientPart :: (MonadRoom m) => RoomWire s e m (Event Client)
clientPart = filterMatch match
  where
    match (ClientPartEvt c) = Just c
    match _ = Nothing

-- | Wire which outputs a client when a client sends a message.
clientMsg :: (MonadRoom m) => RoomWire s e m (Event ClientMsg)
clientMsg = filterMatch match
  where
    match (ClientMsgEvt c) = Just c
    match _ = Nothing


-- | A wire that keeps track of all the clients in the room.
clientList :: (MonadRoom m, Monoid e) => RoomWire s e m [Client]
clientList = hold . accumE (\a f -> f a) [] . updates
  where
    updates = addClient &> delClient
    addClient = arr (fmap (:)) . clientJoin
    delClient = arr (fmap delete) . clientPart


-- | Wire that takes `(Client, MsgData)` tuples and sends them to the
-- appropriate client.
sendMessages :: (MonadRoom m) => Wire s e m (Event (Client, MsgData)) ()
sendMessages = const () <$> onEventM (uncurry sendMessage)

-- | Like `sendMessages`, but automatically converts the second tuple element of
-- the event to a JSON object. Ignores non-objects.
encodeSendMessages :: (MonadRoom m, ToJSON j) => Wire s e m (Event (Client, j)) ()
encodeSendMessages = sendMessages . filterMatch match . arr (fmap (second toJSON))
  where
    match (c, Object o) = Just (c, o)
    match _ = Nothing

-- | Like `sendMessages`, but can send multiple messages at once.
sendMultiMessages :: (MonadRoom m) => Wire s e m (Event [(Client, MsgData)]) ()
sendMultiMessages = const () <$> onEventM (mapM_ (uncurry sendMessage))


-- | Broadcasts the output of the given wire to all clients.
broadcastMsgs :: (MonadRoom m, Monoid e) =>
                 RoomWire s e m (Event MsgData)
              -> RoomWire s e m ()
broadcastMsgs input = sendMultiMessages . applyE (toAll <$> clientList) input
  where
    toAll clients msg = map (, msg) clients


-- | Like `toJSON`, but converts to an `Object`. Crashes if `toJSON` returns
-- something other than an object.
toJsonObj :: ToJSON a => a -> Object
toJsonObj a = case toJSON a of
                Object o -> o
                _ -> error "not an object"


-- | Decodes incoming messages from JSON.
msgsFromJSON :: forall s e m b. (MonadRoom m, FromJSON b) =>
                RoomWire s e m (Event (Client, b))
msgsFromJSON = filterMatch process . clientMsg
  where
    process :: ClientMsg -> Maybe (Client, b)
    process (ClientMsg client msg) =
        let decoded = fromJSON $ Object msg
        in case decoded of
             Success val -> Just (client, val)
             Error _ -> Nothing
