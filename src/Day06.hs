module Day06 (solve) where

import Lude

parse :: String -> ([Int], [Int])
parse = toTuple
      . map (map read . words . dropWhile (not . isDigit))
      . lines

possibilities :: Int -> [Int]
possibilities n = zipWith (*) [0 .. n] [n, n - 1 .. 0]

race :: Int -> Int -> Int
race time dist = length $ filter (> dist) (possibilities time)

run :: ([Int], [Int]) -> Int
run = product . uncurry (zipWith race)

p1 :: String -> Int
p1 = run . parse

p2 :: String -> Int
p2 = run . both (singleton . read . concatMap show) . parse

solve :: AOC
solve = AOC 6 p1 p2
