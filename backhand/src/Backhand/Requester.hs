module Backhand.Requester where

import Control.Concurrent.Chan.Unagi.Bounded

import Backhand.Unique

type Unagi a = (InChan a, OutChan a)

type Requester c = (UniqueRequesterId, Unagi c)

newRequester :: IO (Requester c)
newRequester = pure (,) <*> newRequesterId <*> newChan 10
