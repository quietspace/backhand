module Backhand.Modules.Reversi where

import Backhand
import Backhand.Connection
import Data.Maybe

import Game.Reversi

data Reversi
  = Reversi
    { mid         :: UniqueModId
    , lightPlayer :: Maybe LightPlayer
    , darkPlayer  :: Maybe DarkPlayer
    , rBoard      :: Board
    , turn        :: Player
    }

-- FIXME?: Probably a better way to do this than use pure all the time.
newReversi :: IO Reversi
newReversi = pure Reversi
             <*> newModId
             -- Players aren't set because we need their connection ids.
             <*> pure Nothing
             <*> pure Nothing
             -- Board state stoof.
             <*> pure newBoard
             <*> pure Dark

startReversiGame :: (c -> ReversiMsg) -> OutChan c -> IO ()
startReversiGame fn chan = do
  -- Recursive function that blocks until both player slots are filled.
  (light, dark) <- fillPlayerSlots fn chan

  pure ()

fillPlayerSlots :: (c ->  ReversiMsg) -> OutChan c -> IO (LightPlayer, DarkPlayer)
fillPlayerSlots fn chan = tryFillPlayerSlot Nothing Nothing
  where
    tryFillPlayerSlot :: Maybe LightPlayer -> Maybe DarkPlayer -> IO (LightPlayer, DarkPlayer)
    tryFillPlayerSlot (Just l) (Just d) = pure (l, d)
    tryFillPlayerSlot ml md = do
      rmsg <- readChan chan -- blocks until fufilled
      case fn rmsg of
        (Join Light u) -> if isNothing ml
                          then tryFillPlayerSlot (Just $ LightPlayer u) md
                          else tryFillPlayerSlot ml md
        (Join Dark u) -> if isNothing md
                         then tryFillPlayerSlot ml (Just $ DarkPlayer u)
                         else tryFillPlayerSlot ml md

data LightPlayer
  = LightPlayer UniqueConnId

data DarkPlayer
  = DarkPlayer UniqueConnId

data ReversiMsg
  = Join Player UniqueConnId
