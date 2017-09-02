{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}

module Backhand.Unique where

import GHC.Generics

import Data.Aeson
import Data.Hashable
import Data.UUID
import Data.UUID.Aeson ()
import Control.Concurrent.Unique
import System.Random

newtype ChanUUID =
    ChanUUID UUID
    deriving (Eq, Hashable, Generic)

instance FromJSON ChanUUID
instance ToJSON ChanUUID

newChanUUID :: IO ChanUUID
newChanUUID = fmap ChanUUID randomIO

-- | Unique identification for connections.
newtype UniqueRequester =
    UniqueRequester Unique
    deriving (Eq, Hashable)

newRequesterId :: IO UniqueRequester
newRequesterId = fmap UniqueRequester newUnique
