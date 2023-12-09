module Day09 (solve) where

import Data.List.Extra
import Data.Text qualified as Text
import Lude

parse :: Text -> [[Int]]
parse = map (map read . words . Text.unpack) . Text.lines

diff :: [Int] -> [Int]
diff xs = zipWith (-) (tail xs) xs

next :: [Int] -> Int
next xs
  | allSame xs = last xs
  | otherwise = last xs + next (diff xs)

prev :: [Int] -> [Int]
prev xs
  | allSame xs = [head xs]
  | otherwise = head xs : prev (diff xs)

p1 :: Text -> Int
p1 = sum . map next . parse

p2 :: Text -> Int
p2 = sum . map (foldr (-) 0 . prev) . parse

solve :: AOC
solve = AOC 9 p1 p2
