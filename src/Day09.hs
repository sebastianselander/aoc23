module Day09 (solve) where

import Data.List.Extra
import Data.Text qualified as Text (lines, unpack)
import Lude

step :: ([Int] -> Int) -> [Int] -> [Int]
step f xs | allSame xs = [f xs]
          | otherwise = f xs : step f (zipWith (-) (tail xs) xs)

sol :: ([Int] -> Int) -> ([Int] -> Int) -> Text -> Int
sol f g = sum . map ((f . step g) . map read . words . Text.unpack) . Text.lines

p1 :: Text -> Int
p1 = sol sum last

p2 :: Text -> Int
p2 = sol diffr head

solve :: AOC
solve = AOC 9 p1 p2
