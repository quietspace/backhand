{-# LANGUAGE Arrows #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Backhand.Behavior.Reversi
    ( reversiBehavior
    ) where

import Control.Applicative -- Implicit in GHC 7.10
import Control.Auto
import Control.Auto.Blip
import Control.Monad
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Matrix as Ma
import Data.Maybe

import Prelude hiding (id, (.))

import Backhand.Room
import Backhand.Behavior.Players
import Backhand.Util.Auto


-- | Represents a position on the board.
data Position = Position Int Int
     deriving (Show, Eq)

-- | Add two positions.
(+.) :: Position -> Position -> Position
(Position x1 y1) +. (Position x2 y2) = Position (x1+x2) (y1+y2)

-- | Get the state of the given position on the board.
(!.) :: Board -> Position -> Cell
(Board m) !. (Position x y)
    | x <= 8 && x > 0 && y <= 8 && y > 0 = Ma.getElem y x m
    | otherwise = Empty

-- | Represents a straight line of cells on the board. Defined by a starting
-- position, direction, and cell count.
data Line = Line Position Direction Int

-- | Represents a direction.
data Direction = N | NE | E | SE | S | SW | W | NW
     deriving (Show, Eq, Enum)

-- | Represents one of the players in reversi, either light or dark.
data Player = Light | Dark
     deriving (Show, Eq, Enum, Ord, Bounded)

-- | Represents a cell on the board. Either a piece of a certain color or empty.
data Cell = Piece Player | Empty
     deriving (Show, Eq)

-- | Represents the entire reversi board.
data Board = Board (Ma.Matrix Cell)
     deriving (Show, Eq)

-- | Bulk-update several cells at once.
-- FIXME: Currently this is done _very_ inefficiently.
setCells :: [(Position, Player)] -> Board -> Board
setCells pts (Board board) = Board $ sc pts board
  where
    sc [] b = b
    sc ((Position x y, pl):ps) b = sc ps (Ma.setElem (Piece pl) (y, x) b)


-- | Returns a board in the initial game state.
initBoard :: Board
initBoard = Board $ Ma.matrix 8 8 genBoard
  where
    genBoard (4, 4) = Piece Light
    genBoard (5, 4) = Piece Dark
    genBoard (5, 5) = Piece Light
    genBoard (4, 5) = Piece Dark
    genBoard _ = Empty


-- | Given a player color, return the opposite.
opposite :: Player -> Player
opposite Light = Dark
opposite Dark = Light


-- | Given a player and a position, find all of the possible lines from the
-- given position on the board. Returns a list of end points for those lines.
findLines :: Player -> Position -> Board -> [Line]
findLines pl from board =
    mapMaybe (\d -> findLine pl from d board) allDirections


-- | Attempt to find a line of pieces from one piece of the given color in the
-- given direction. This is the line of pieces which would be flipped if a piece
-- of the given color were placed at the given position.
findLine :: Player -> Position -> Direction -> Board -> Maybe Line
findLine pl from dir board =
    -- There must be at least one opposing piece for the move to be valid.
    if board !. (from +. dv) == Piece (opposite pl)
       then doFind (from +. dv) 1
       else Nothing
  where
    -- Finds the end of the chain.
    doFind' :: Position -> Int -> Cell -> Maybe Line
    -- If the piece at this position is of the opposite player, move on to
    -- the next position.
    -- If it is of the same player, we've found the end of the chain.
    doFind' pos ctr (Piece p) | p == opposite pl = doFind (pos +. dv) (ctr + 1)
                              | otherwise = Just $ Line from dir ctr
    doFind' _ _ _ = Nothing
    -- Like doFind', but checks if the position is within the board's boundaries.
    doFind pos ctr | inBounds pos board = doFind' pos ctr (board !. pos)
                   | otherwise = Nothing
    dv = dirVector dir


-- | Generate a list of all positions in the given line (excluding the first).
linePoints :: Line -> [Position]
linePoints (Line from dir len) = doLP (from +. dv) (len-1)
  where
    doLP _ 0 = []
    doLP pos ctr = pos : doLP (pos +. dv) (ctr-1)
    dv = dirVector dir

         
-- | Flips all of the pieces in the given line to the given player's color.
flipLine :: Player -> Line -> Board -> Board
flipLine pl line board = setCells setList board
  where
    setList = map (, pl) points
    points = linePoints line


-- | Performs the given move at the given position and calculates the new state
-- of the game board. If the move is illegal, returns `Left` with an error
-- message.
tryMove :: Player -> Position -> Board -> Either T.Text Board
tryMove pl pos board
    -- We can't place pieces on top of others.
    | board !. pos /= Empty = Left "That space is occupied."
    -- There must be at least one line of pieces.
    | length rlines > 0 =
        Right $ setCells [(pos, pl)] $ foldr (flipLine pl) board rlines
    | otherwise = Left "That is not a valid move."
  where
    rlines = findLines pl pos board

-- | True if the given position is inside the boundaries of the given board.
inBounds :: Position -> Board -> Bool
inBounds (Position x y) (Board b) =
    x > 0 && x <= Ma.ncols b && y > 0 && y <= Ma.nrows b

-- | Returns a position that is one tile away from @(0, 0)@ in the given
-- direction.
dirVector :: Direction -> Position
dirVector N  = Position ( 0) (-1)
dirVector NE = Position ( 1) (-1)
dirVector  E = Position ( 1) ( 0)
dirVector SE = Position ( 1) ( 1)
dirVector S  = Position ( 0) ( 1)
dirVector SW = Position (-1) ( 1)
dirVector  W = Position (-1) ( 0)
dirVector NW = Position (-1) (-1)

-- | A list of every direction.
allDirections :: [Direction]
allDirections = [N, NE, E, SE, S, SW, W, NW]



-- | An auto which takes a stream of moves and outputs either the game board's
-- state after the given move or an error message.
gameBoard :: Auto' (Player, Position) (Either T.Text Board)
gameBoard = mkState_ updateBoard initBoard
  where
    updateBoard (pl, pos) board =
        either (\err -> (Left err, board))
               (\newBoard -> (Right newBoard, newBoard))
               (tryMove pl pos board)


firstTurn :: Player
firstTurn = Light


reversiBehavior :: RoomBehavior
reversiBehavior = proc evt -> do
  rec
    -- Player slots.
    (slots, slotMsgs) <- identSlots -< evt
    lightSlotI <- getSlot Light -< slots
    darkSlotI  <- getSlot Dark  -< slots

    -- The current turn. This is simply an accumulator that flips back and forth
    -- between `Light` and `Dark` each time a turn ends. Delayed to prevent
    -- infinite recursion.
    turn <- scanBD_ (\t _ -> opposite t) firstTurn -< turnEndB

    -- Moves made by the `Light` player.
    lightMoveB <- movesFor Light -< (lightSlotI, evt)
    -- Moves made by the `Dark` player.
    darkMoveB <- movesFor Dark -< (darkSlotI, evt)

    let moveB :: Blip (Player, Position)
        moveB = mergeL lightMoveB darkMoveB

    -- Moves made by the player whose turn it is, tagged with that player's ID
    -- (`Light` or `Dark`).
    let turnMoveInput = (turn, ) <$> moveB
    turnMoveB' <- filterB (\(pl, (mpl, _)) -> pl == mpl) -< turnMoveInput
    let turnMoveB = snd <$> turnMoveB'

    -- Blip streams containing error messages or board updates.
    -- TODO: Send move errors to clients.
    (moveErrorB, boardUpdateB) <-
        joinBothB <$> perBlip (onEithers <<< gameBoard) -< turnMoveB

    -- Emits a blip when the current player's turn is over. This is either when
    -- the player makes a move or when the player cannot make a move.
    -- TODO: Implement turn skipping.
    turnEndB <- tagBlips () -< boardUpdateB

    -- Tag board update blips with the current turn. Note that since the @turn@
    -- value is delayed one tick, we send the opposite of its current value to
    -- our clients.
    let stateUpdateB = (, opposite turn) <$> boardUpdateB
    updateMsgB <- perBlip stateUpdateMsgs -< stateUpdateB

    clients <- clientList -< evt

    -- A list of message send tuples which will tell backhand to send the update
    -- messages to all the clients.
    let sendUpdateB :: Blip [(Client, MsgData)]
        sendUpdateB = (\msg -> map (, msg) clients) <$> updateMsgB
    sendUpdates <- sendMultiMessages -< sendUpdateB

    sendInitial <- sendInitialState -< (evt, stateUpdateB)
  returnA -< (sendUpdates <> sendInitial <> slotMsgs)


-- | Takes a stream client events and a blip stream of game state updates and
-- outputs tuple lists to send the game state to each client joining the room.
sendInitialState :: Auto' (ClientEvent, Blip (Board, Player)) [(Client, MsgData)]
sendInitialState = proc (evt, stateUpdateB) -> do
  -- When a client joins, we should send them the game state. To do this, we
  -- first create an auto which will hold the last state message sent.
  lastStateUpdate <- holdWith_ (initBoard, firstTurn) -< stateUpdateB
  stateUpdateMsg <- stateUpdateMsgs -< lastStateUpdate

  -- Now, when a client joins, we send them the current `lastStateUpdate`.
  clientJoinB <- clientJoin -< evt

  let msgB = (, stateUpdateMsg) <$> clientJoinB
  sendMessages -< msgB


-- | An auto which outputs a blip stream of move requests from the given message
-- data stream.
moveRequests :: Auto' MsgData (Blip Position)
moveRequests = decodeMsgsWith parseMove
  where
    parseMove (MsgData "make-move" v) =
        Position <$> v .: "col"
                 <*> v .: "row"
    parseMove _ = mzero

-- | Given a player ID and its associated slot, produces an auto which takes
-- client events and outputs a blip stream of tuples representing move requests
-- made by that player.
movesFor :: Player -> Auto' (PlayerSlot, ClientEvent) (Blip (Player, Position))
movesFor pl = proc (slot, evt) -> do
  playerMsgB <- playerMessages -< (slot, evt)
  moveReqB <- joinB <<< perBlip moveRequests -< playerMsgB
  modifyBlips (pl, ) -< moveReqB


-- | An auto which transforms a stream of `(Board, Player)` tuples into state
-- update messages.
stateUpdateMsgs :: Auto' (Board, Player) MsgData
stateUpdateMsgs = arr $ \((Board m), pl) ->
    msgData "game-state"
            [ "board" .= Ma.toLists m
            , "turn" .= pl ]

-- | Move requests tagged with the color of the player sending them.
-- taggedMoves :: Players -> RoomAuto (Blip (Player, Position))
-- taggedMoves (Players darkPlayer lightPlayer) =
--     tagged Dark darkPlayer &> tagged Light lightPlayer
--   where
--     tagged :: Player -> PlayerSlot -> RoomAuto (Blip (Player, Position))
--     tagged tag player = perBlip (arr (tag, )) <<< moveReqs player
--     moveReqs :: PlayerSlot -> RoomAuto (Blip Position)
--     moveReqs p = modifyBlips moveReqPos
--                  <<< joinB <<< perBlip decodeMessages
--                  <<< playerMessages p


-------- JSON encoding/decoding stuff --------

instance ToJSON Player where
    toJSON Dark = String "dark"
    toJSON Light = String "light"

instance FromJSON Player where
    parseJSON (String "dark") = return Dark
    parseJSON (String "light") = return Light
    parseJSON _ = mzero


instance ToJSON Cell where
    toJSON (Piece Dark)  = String "dark"
    toJSON (Piece Light) = String "light"
    toJSON Empty         = String "empty"

