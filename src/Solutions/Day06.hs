module Solutions.Day06 (solve) where

import Lude
import Data.Text qualified as Text

data Race = R { time :: [Int], distance :: [Int] }
  deriving Show

parse :: Text -> Race
parse t = R (fixup time) (fixup dist)
  where
    [time,dist] = map Text.unpack $ Text.lines t
    fixup = map read . words . dropWhile (not . isDigit)

possibilities :: Int -> [Int]
possibilities n = zipWith (*) [0 .. n ] [n, n - 1 .. 0]

race :: Int -> Int -> Int
race time dist = length $ filter (>dist) (possibilities time)

p1 :: Text -> Int
p1 t = product $ zipWith race (time r) (distance r)
  where
    r = parse t

p2 :: Text -> Int
p2 t = product 
     $ zipWith race [read $ concatMap show ti] [read $ concatMap show di]
    where
      (R ti di) = parse t

solve :: AOC
solve = AOC 6 p1 p2
