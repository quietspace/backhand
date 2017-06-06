{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveGeneric,
  DeriveTraversable #-}

module Backhand.Message where

import GHC.Generics

import Backhand.Unique

-- | It's common to tag a message with the requester's unique id and we can make
-- this simple to think about by showing that it is a Functor. This lets us keep
-- `UniqueRequesterId` out of every constructer that requires information about
-- who sent us this message.
type Message m = (UniqueRequesterId, m)

req :: Message m -> UniqueRequesterId
req = fst

msg :: Message m -> m
msg = snd

data ConnectionData c = ConnectionData
    {
      -- | Hashed room unique to allow the connector to know where to send the
      -- message back to.
      conRoomId :: UniqueChanId
    ,
      -- | Type for modules to filter out if the message is intended for them.
      conModId :: UniqueModuleId
    ,
      -- | Package message for the client/module.
      conMessage :: c
    } deriving (Functor,Foldable,Traversable,Generic)
