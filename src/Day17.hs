module Day17 (solve) where

import Algorithm.Search (aStar)
import Data.Matrix hiding (inverse, trace)
import Data.Text (unpack)
import Lude hiding (Any)

data Direction = North | West | South | East | Any
    deriving (Eq, Ord)

type State = (Int, Int, Int, Direction)

inverse :: Direction -> Direction
inverse = \case
    North -> South
    South -> North
    West -> East
    East -> West
    Any -> Any

upto :: Int -> Int -> Int
upto n m = n `mod` (m + 1)

parse :: Text -> Matrix Int
parse = fromLists . map (map digitToInt) . lines . unpack

pathCost
    :: ((Int, Int) -> State -> [State])
    -> Matrix Int
    -> Maybe (Int, [State])
pathCost f m = aStar (f endPoint) cost manh end start
  where
    start = (1, 1, 0, Any)
    endPoint = (nrows m, ncols m)
    end (row, col, _, _) = (row, col) == endPoint
    manh (row, col, _, _) = manhattan (row, col) endPoint
    cost _ (row, col, _, _) = getElem row col m

neighbs :: Int -> (State -> Bool) -> (Int, Int) -> State -> [State]
neighbs max condition (rowBound, colBound) s@(row, col, step, dir) =
    if condition s
        then filter (\(_, _, _, dir') -> dir == dir') all
        else all
  where
    all =
        filter
            (inside &.& available)
            [
                ( row + 1
                , col
                , bool 0 (succ step `upto` max) (dir == South)
                , South
                )
            ,
                ( row - 1
                , col
                , bool 0 (succ step `upto` max) (dir == North)
                , North
                )
            ,
                ( row
                , col - 1
                , bool 0 (succ step `upto` max) (dir == West)
                , West
                )
            ,
                ( row
                , col + 1
                , bool 0 (succ step `upto` max) (dir == East)
                , East
                )
            ]
    inside (x, y, _, _) =
        x >= 1 && x <= rowBound && y >= 1 && y <= colBound
    available (_, _, step', dir') =
        (dir /= dir' || step' < max) && dir' /= inverse dir

p1Cond, p2Cond :: State -> Bool
p1Cond = const False
p2Cond (_, _, step, dir) = step < 3 && dir /= Any

-- ~ 4 seconds
p1 :: Text -> Int
p1 = fst . fromJust . pathCost (neighbs 3 p1Cond) . parse

-- ~ 19 seconds
p2 :: Text -> Int
p2 = fst . fromJust . pathCost (neighbs 10 p2Cond) . parse

solve :: AOC
solve = AOC 17 p1 p2
