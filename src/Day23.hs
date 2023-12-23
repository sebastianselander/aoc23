module Day23 (solve) where

import Control.Parallel.Strategies
import Data.Matrix hiding (trace)
import Data.Set (Set)
import Data.Set qualified as Set
import Lude

type Coord = (Int, Int)

parse :: String -> Matrix Char
parse = fromLists . lines

neighbours :: Matrix Char -> Bool -> Coord -> [Coord]
neighbours matrix b coord
    | b = filter viable $ case matrix !? coord of
        Just '>' -> [right coord]
        Just '<' -> [left coord]
        Just 'v' -> [below coord]
        _ -> [above coord, left coord, below coord, right coord]
    | otherwise = filter viable [above coord, left coord, below coord, right coord]
  where
    viable coord' = case matrix !? coord' of
        Nothing -> False
        Just '#' -> False
        _ -> True

dfs :: Bool -> Matrix Char -> Coord -> Int
dfs b matrix = go 0 mempty
  where
    go :: Int -> Set Coord -> Coord -> Int
    go acc visited coord
        | Set.member coord visited = 0
        | coord == (nrows matrix, ncols matrix - 1) = acc
        | otherwise =
            maximum $
                parMap
                    rdeepseq
                    (go (acc + 1) (Set.insert coord visited))
                    (neighbours matrix b coord)

p1 :: String -> Int
p1 s = dfs True (parse s) (1, 2)

p2 :: String -> Int
p2 s = dfs False (parse s) (1, 2)

solve :: AOC
solve = AOC 23 p1 p2

-- + 54
