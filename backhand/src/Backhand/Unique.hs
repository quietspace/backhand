{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Backhand.Unique where
s
import Data.Hashable
import Control.Concurrent.Unique

-- | Unique identification for `Channel`s.
newtype UniqueChanId =
    UniqueChanId Unique
    deriving (Eq,Ord,Hashable)

newChanId :: IO UniqueChanId
newChanId = fmap UniqueChanId newUnique

-- | Unique identification for modules.
newtype UniqueModuleId =
    UniqueModuleId Unique
    deriving (Eq,Ord,Hashable)

newModuleId :: IO UniqueModuleId
newModuleId = fmap UniqueModuleId newUnique

-- | Unique identification for connections.
newtype UniqueRequesterId =
    UniqueRequesterId Unique
    deriving (Eq,Ord,Hashable)

newRequesterId :: IO UniqueRequesterId
newRequesterId = fmap UniqueRequesterId newUnique
