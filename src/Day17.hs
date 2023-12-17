module Day17 (solve) where

import Algorithm.Search (aStar)
import Data.Matrix (Matrix, fromLists, getElem, ncols, nrows)
import Data.Text (unpack)
import Lude

data Direction = North | West | South | East | Whatever
    deriving (Eq, Ord)

type State = (Int, Int, Int, Direction)

inverse :: Direction -> Direction
inverse = \case
    North -> South
    South -> North
    West -> East
    East -> West
    Whatever -> Whatever

parse :: Text -> Matrix Int
parse = fromLists . map (map digitToInt) . lines . unpack

pathCost
    :: ((Int, Int) -> State -> [State])
    -> Matrix Int
    -> Maybe (Int, [State])
pathCost f m = aStar (f endPoint) cost manh end start
  where
    start = (1, 1, 0, Whatever)
    endPoint = (nrows m, ncols m)
    end (row, col, _, _) = (row, col) == endPoint
    manh (row, col, _, _) = manhattan (row, col) endPoint
    cost _ (row, col, _, _) = getElem row col m

neighbors :: Int -> Int -> (Int, Int) -> State -> [State]
neighbors max min (rowBound, colBound) (row, col, step, dir)
    | step < (min - 1) && dir /= Whatever =
        filter (\(_, _, _, dir') -> dir == dir') all
    | otherwise = all
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
    upto n m = n `mod` (m + 1)
    inside (x, y, _, _) = x >= 1 && x <= rowBound && y >= 1 && y <= colBound
    available (_, _, step', dir') =
        (dir /= dir' || step' < max) && dir' /= inverse dir

-- ~ 4 seconds
p1 :: Text -> Int
p1 = fst . fromJust . pathCost (neighbors 3 1) . parse

-- ~ 19 seconds
p2 :: Text -> Int
p2 = fst . fromJust . pathCost (neighbors 10 4) . parse

solve :: AOC
solve = AOC 17 p1 p2
