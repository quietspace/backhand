{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Utility functions for aeson.
module Backhand.Util.Aeson where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.HashMap.Strict as H


-- | Converts a list of aeson pairs to an object.
jsonObj :: [Pair] -> Object
jsonObj = H.fromList

-- | Typeclass for objects which can be converted to JSON objects. This is like
-- the `ToJSON` class, but disallows anything but an object.
class ToJSONObject a where
    toJSONObj :: a -> Object
