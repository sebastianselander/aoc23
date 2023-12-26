module Day21 (solve) where

import Control.Monad.State
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Matrix hiding (trace)
import Data.Vector qualified as Vec
import Lude hiding ((<|>))

type Coord = (Int, Int)

parse :: String -> Matrix Char
parse = fromLists . lines

startPos :: Matrix Char -> Maybe Coord
startPos m = second succ <$> go (nrows m) m
  where
    go :: Int -> Matrix Char -> Maybe (Int, Int)
    go (-1) _ = Nothing
    go n mat = do
        vec <- safeGetRow n mat
        case Vec.elemIndex 'S' vec of
            Nothing -> go (n - 1) mat
            Just i -> Just (n, i)

reachable :: Int -> Matrix Char -> Map Coord Int
reachable n matrix = execState (go [(1, start)]) (Map.singleton start 0)
  where
    start = fromJust $ startPos matrix
    go :: [(Int, Coord)] -> State (Map Coord Int) ()
    go [] = pure ()
    go ((m, coord) : xs)
        | n + 1 == m = pure ()
        | otherwise = do
            visited <- get
            let visit =
                    [ coord'
                    | coord' <-
                        [ above coord
                        , left coord
                        , below coord
                        , right coord
                        ]
                    , Just '.' == matrix !? coord'
                    , Map.notMember coord' visited
                    ]
            put (foldr (`Map.insert` m) visited visit)
            go (xs ++ [(m + 1, v) | v <- visit])

p1 :: String -> Int
p1 = length . filter even . Map.elems . reachable 64 . parse

-- Disgusting
p2 :: String -> Int
p2 s =
    ((n + 1) * (n + 1)) * odds
        + (n * n) * evens
        - (n + 1) * oddCorners
        + n * evenCorners
  where
    lengthMap = reachable maxBound $ parse s
    evenCorners = length $ filter (\x -> even x && x > 65) $ Map.elems lengthMap
    oddCorners = length $ filter (\x -> odd x && x > 65) $ Map.elems lengthMap
    odds = length $ filter odd $ Map.elems lengthMap
    evens = length $ filter even $ Map.elems lengthMap
    n = 202300

solve :: AOC
solve = AOC 21 p1 p2
