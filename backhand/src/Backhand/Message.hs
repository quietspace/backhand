{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveGeneric,
  DeriveTraversable, DuplicateRecordFields #-}

module Backhand.Message where

import GHC.Generics

import Control.Concurrent.Unique
import Data.Aeson
import Data.UUID

-- | Internal messaging to and from a Requester
data Message c = Message
  { unique :: Unique
  , message :: c
  } deriving (Functor, Foldable, Traversable, Generic)

-- | Communication within backhand doesn't have to be 1 to 1 so a requester may
-- be present in multiple `Channel`s at once; to simplify the process
-- `ConnectionData` can be used to identify a `Channel` within a `ChannelMap`
-- and then a `Module` by it's `Text` within the `Channel`.
data ConnectionData t c = ConnectionData
  { uuid :: UUID
  , service :: t
  , message :: c
  } deriving (Functor, Foldable, Traversable, Generic)

instance (FromJSON t, FromJSON c) => FromJSON (ConnectionData t c)
instance (ToJSON t, ToJSON c) => ToJSON (ConnectionData t c)
