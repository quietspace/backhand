{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Game.Reversi where

import           Data.Matrix (Matrix)
import qualified Data.Matrix as Ma
import           Data.Maybe
import           Data.Monoid
import           Data.Text   (Text)

data Player
  = Light
  | Dark
  deriving (Eq, Enum, Ord, Bounded)

nextPlayer :: Player -> Player
nextPlayer Light = Dark
nextPlayer Dark = Light

data Position
  = Position Int Int
  deriving (Eq)

instance Monoid Position where
  mempty = Position 0 0
  mappend (Position x1 y1) (Position x2 y2)
    = Position (x1 + x2) (y1 + y2)

data Cell
  = Piece Player
  | Empty
  deriving (Eq)

newtype Board
  = Board (Matrix Cell)

(!.) :: Board -> Position -> Cell
(Board m) !. (Position x y)
    | x <= 8 && x > 0 && y <= 8 && y > 0 = Ma.getElem y x m
    | otherwise = Empty

data Line
  = Line Position Direction Int

-- | Given a player and a position, find all of the possible lines from the
-- given position on the board. Returns a list of end points for those lines.
findLines :: Player -> Position -> Board -> [Line]
findLines pl from board =
    mapMaybe (\d -> findLine pl from d board) [N .. NW]

-- | Attempt to find a line of pieces from one piece of the given color in the
-- given direction. This is the line of pieces which would be flipped if a piece
-- of the given color were placed at the given position.
findLine :: Player -> Position -> Direction -> Board -> Maybe Line
findLine pl from dir board =
    -- There must be at least one opposing piece for the move to be valid.
    if board !. (from <> dv) == Piece (nextPlayer pl)
       then doFind (from <> dv) 1
       else Nothing
  where
    -- Finds the end of the chain.
    doFind' :: Position -> Int -> Cell -> Maybe Line
    -- If the piece at this position is of the opposite player, move on to
    -- the next position.
    -- If it is of the same player, we've found the end of the chain.
    doFind' pos ctr (Piece p) | p == nextPlayer pl = doFind (pos <> dv) (ctr + 1)
                              | otherwise = Just $ Line from dir ctr
    doFind' _ _ _ = Nothing
    -- Like doFind', but checks if the position is within the board's boundaries.
    doFind pos ctr | inBounds pos board = doFind' pos ctr (board !. pos)
                   | otherwise = Nothing
    dv = dirVector dir

-- | Flips all of the pieces in the given line to the given player's color.
flipLine :: Player -> Line -> Board -> Board
flipLine pl line = setCells setList
  where
    setList = map (, pl) points
    points = linePoints line


data Direction
  = N | NE | E | SE | S | SW | W | NW
  deriving (Eq, Enum, Ord, Bounded)

-- | Returns a position that is one tile away from @(0, 0)@ in the given
-- direction.
dirVector :: Direction -> Position
dirVector N  = Position   0  (-1)
dirVector NE = Position   1  (-1)
dirVector  E = Position   1    0
dirVector SE = Position   1    1
dirVector S  = Position   0    1
dirVector SW = Position (-1)   1
dirVector  W = Position (-1)   0
dirVector NW = Position (-1) (-1)

newBoard :: Board
newBoard = Board $ Ma.matrix 8 8 genBoard
  where
    genBoard (4, 4) = Piece Light
    genBoard (5, 4) = Piece Dark
    genBoard (5, 5) = Piece Light
    genBoard (4, 5) = Piece Dark
    genBoard _ = Empty

-- | Performs the given move at the given position and calculates the new state
-- of the game board. If the move is illegal, returns `Left` with an error
-- message.
tryMove :: Player -> Position -> Board -> Either Text Board
tryMove pl pos board
    -- We can't place pieces on top of others.
    | board !. pos /= Empty = Left "That space is occupied."
    -- There must be at least one line of pieces.
    | not (null rlines) =
        Right $ setCells [(pos, pl)] $ foldr (flipLine pl) board rlines
    | otherwise = Left "That is not a valid move."
  where
    rlines = findLines pl pos board

-- | True if the given position is inside the boundaries of the given board.
inBounds :: Position -> Board -> Bool
inBounds (Position x y) (Board b) =
    x > 0 && x <= Ma.ncols b && y > 0 && y <= Ma.nrows b

-- | Bulk-update several cells at once.
-- FIXME: Currently this is done _very_ inefficiently.
setCells :: [(Position, Player)] -> Board -> Board
setCells pts (Board board) = Board $ sc pts board
  where
    sc [] b = b
    sc ((Position x y, pl):ps) b = sc ps (Ma.setElem (Piece pl) (y, x) b)

-- | Generate a list of all positions in the given line (excluding the first).
linePoints :: Line -> [Position]
linePoints (Line from dir len) = doLP (from <> dv) (len-1)
  where
    doLP _ 0 = []
    doLP pos ctr = pos : doLP (pos <> dv) (ctr-1)
    dv = dirVector dir
