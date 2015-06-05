{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Backhand.Room
    ( RoomBehavior
    , Client (cId)
    -- * Messages
    , ClientMsg (..)
    , MsgData
    , sendMessages
    , sendMultiMessages
    , eventsFromJSON
    ) where

import Control.Applicative -- Implicit in GHC 7.10
import Control.Concurrent.STM
import Control.Concurrent.STM.TRLock
import Data.Aeson
import Reactive.Banana
import Reactive.Banana.Frameworks

import Backhand.Room.Types

-------- Event Handler Stuff --------

-- | The `RoomBehavior` defines a room's actual behavior in response to
-- messages. It consists of a function which takes a set of events and behaviors
-- as arguments and compiles an event network which should implement the desired
-- behavior.
type RoomBehavior = AddHandler ClientMsg -> AddHandler [Client] -> IO EventNetwork


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
