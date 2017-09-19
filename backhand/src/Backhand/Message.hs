{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveGeneric,
  DeriveTraversable #-}

module Backhand.Message where

import GHC.Generics

import Data.Aeson
import Data.Text

import Backhand.Unique

-- | It's common to tag a message with the requester's unique id and we can make
-- this simple to think about by showing that it is a Functor. This lets us keep
-- `UniqueRequesterId` out of every constructer that requires information about
-- who sent us this message.
type Message m = (UniqueRequester, m)

data ConnectionData c = ConnectionData ChanUUID Text c
  deriving (Functor, Foldable, Traversable, Generic)

instance FromJSON c => FromJSON (ConnectionData c)
instance ToJSON c => ToJSON (ConnectionData c)
