{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}

module Backhand.Unique where

import GHC.Generics

import Data.Aeson
import Data.Hashable
import Data.UUID
import Data.Vector
import Control.Concurrent.Unique
import System.Random

newtype ChanUUID =
    ChanUUID UUID
    deriving (Eq, Hashable, Generic)

instance FromJSON ChanUUID where
  parseJSON = withArray "ChanUUID" $ \a ->
    case a !? 0 of
      Just v -> withText "UUID"
        ( \t ->
            case fromText t of
              Just u -> pure $ ChanUUID u
              Nothing -> fail "Expected UUID encountered invalid UUID format"
        ) v
      Nothing -> fail "Expected ChanUUID encountered empty array"

instance ToJSON ChanUUID where
  toEncoding (ChanUUID uuid) =
    foldable [toJSON $ toText uuid]

newChanUUID :: IO ChanUUID
newChanUUID = fmap ChanUUID randomIO

-- | Unique identification for connections.
newtype UniqueRequester =
    UniqueRequester Unique
    deriving (Eq, Hashable)

newRequesterId :: IO UniqueRequester
newRequesterId = fmap UniqueRequester newUnique
