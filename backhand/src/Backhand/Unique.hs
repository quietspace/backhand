module Backhand.Unique where

import Data.Hashable
import Data.Unique

-- | Unique identification for `Channel`s.
newtype UniqueChanId
  = UniqueChanId Unique
  deriving (Eq, Ord)

-- FIXME: Switch to DeriveGeneric once Unique's instance lands in hashable.
instance Hashable UniqueChanId where
  hash (UniqueChanId u) = hashUnique u
  hashWithSalt = hashUsing hash

newChanId :: IO UniqueChanId
newChanId = fmap UniqueChanId newUnique

-- | Unique identification for modules.
newtype UniqueModId
  = UniqueModId Unique
  deriving (Eq, Ord)

-- FIXME: Switch to DeriveGeneric once Unique's instance lands in hashable.
instance Hashable UniqueModId where
  hash (UniqueModId u) = hashUnique u
  hashWithSalt = hashUsing hash

newModId :: IO UniqueModId
newModId = fmap UniqueModId newUnique

-- | Unique identification for connections.
newtype UniqueConnId
  = UniqueConnId Unique
  deriving (Eq, Ord)

-- FIXME: Switch to DeriveGeneric once Unique's instance lands in hashable.
instance Hashable UniqueConnId where
  hash (UniqueConnId u) = hashUnique u
  hashWithSalt = hashUsing hash

newConnId :: IO UniqueConnId
newConnId = fmap UniqueConnId newUnique
