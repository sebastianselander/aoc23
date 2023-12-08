module Day06 (solve) where

import Data.Text (unpack)
import Lude

parse :: Text -> ([Int], [Int])
parse = listToTuple
      . map (map read . words . dropWhile (not . isDigit))
      . lines
      . unpack

possibilities :: Int -> [Int]
possibilities n = zipWith (*) [0 .. n] [n, n - 1 .. 0]

race :: Int -> Int -> Int
race time dist = length $ filter (> dist) (possibilities time)

run :: ([Int], [Int]) -> Int
run = product . uncurry (zipWith race)

p1 :: Text -> Int
p1 = run . parse

p2 :: Text -> Int
p2 = run . both (singleton . read . concatMap show) . parse

solve :: AOC
solve = AOC 6 p1 p2
