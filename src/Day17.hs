module Day17 (solve) where

import Algorithm.Search (aStar)
import Data.Matrix (Matrix, fromLists, getElem, ncols, nrows)
import Lude

data Direction = North | West | South | East | Whatever
    deriving (Eq, Ord, Show)

type S = (Int, Int, Int, Direction) -- Row, Col, Steps, Direction

inverse :: Direction -> Direction
inverse = \case
    North -> South
    South -> North
    West -> East
    East -> West
    Whatever -> Whatever

parse :: String -> Matrix Int
parse = fromLists . map (map digitToInt) . lines

path
    :: ((Int, Int) -> S -> [S])
    -> Matrix Int
    -> Maybe (Int, [S])
path f m = aStar (f endPoint) cost manh end start
  where
    start = (1, 1, 0, Whatever)
    endPoint = (nrows m, ncols m)
    end (row, col, _, _) =  (row, col) == endPoint
    manh (row, col, _, _) =  manhattan (row, col) endPoint
    cost _ (row, col, _, _) =  getElem row col m

neighbors :: Int -> Int -> (Int, Int) -> S -> [S]
neighbors max min (rowBound, colBound) (!row, !col, !step, !dir) =
        [ s
        | s@(row', col', step', dir') <-
            [ (row + 1, col, g South, South)
            , (row - 1, col, g North, North)
            , (row, col - 1, g West, West)
            , (row, col + 1, g East, East)
            ]
        , (dir /= dir' || step' < max) && dir' /= inverse dir
        , row' >= 1 && row' <= rowBound && col' >= 1 && col' <= colBound
        , not (step < (min - 1) && dir /= Whatever) || (dir == dir')
        ]
  where
    upto n m = n `mod` (m + 1)
    g dir' = bool 0 (succ step `upto` max) (dir == dir')

p1 :: String -> Int
p1 = fst . fromJust . path (neighbors 3 1) . parse

p2 :: String -> Int
p2 = fst . fromJust . path (neighbors 10 4) . parse

solve :: AOC
solve = AOC 17 p1 p2
