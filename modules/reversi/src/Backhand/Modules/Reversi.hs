module Backhand.Modules.Reversi where

import Backhand
import Backhand.Message
import Backhand.Unique
import Control.Concurrent.Chan.Unagi.Bounded
import Data.Maybe

import Game.Reversi

data Reversi = Reversi
    { lightPlayer :: LightPlayer
    , darkPlayer :: DarkPlayer
    , rBoard :: Board
    , turn :: Player
    }

newReversi :: (LightPlayer, DarkPlayer) -> IO Reversi
newReversi (lp,dp) =
    pure Reversi <*> pure lp <*> pure dp <*> pure newBoard <*>
    pure Dark

-- startReversiGame :: (c -> Message ReversiMsg) -> OutChan c -> IO ()
-- startReversiGame fn chan =
--     -- Recursive function that blocks until both player slots are filled.
--     fillPlayerSlots
--         fn
--         chan
--     -- Take the players and generate a new game.
--     >>=
--     newReversi
--     -- Now let the games begin.
--     >>=
--     reversiGameLoop chan

fillPlayerSlots :: (c -> Message ReversiMsg)
                -> OutChan c
                -> IO (LightPlayer, DarkPlayer)
fillPlayerSlots fn chan = tryFillPlayerSlot Nothing Nothing
  where
    tryFillPlayerSlot :: Maybe LightPlayer
                      -> Maybe DarkPlayer
                      -> IO (LightPlayer, DarkPlayer)
    -- Once we have both player slots filled we'll exit with those players.
    tryFillPlayerSlot (Just l) (Just d) = pure (l, d)
    tryFillPlayerSlot ml md = do
        rmsg <- readChan chan -- blocks until fufilled
        case fn rmsg of
            (u,Join Light) ->
                case ml of
                    -- Fill the player slot
                    -- TODO: Broadcast the new player
                    Nothing -> tryFillPlayerSlot (Just $ LightPlayer u) md
                    -- Already exists
                    -- TODO: Return a warning to the sender
                    Just _ -> tryFillPlayerSlot ml md
            (u,Join Dark) ->
                case md of
                    Nothing -> tryFillPlayerSlot ml (Just $ DarkPlayer u)
                    Just _ -> tryFillPlayerSlot ml md

-- reversiGameLoop :: OutChan c -> Reversi -> IO ()
-- reversiGameLoop chan game = _

data LightPlayer
  = LightPlayer UniqueRequester

data DarkPlayer
  = DarkPlayer UniqueRequester

data ReversiMsg
  = Join Player
