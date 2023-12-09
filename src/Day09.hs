module Day09 (solve) where

import Data.List.Extra
import Data.Text qualified as Text (lines, unpack)
import Lude

step :: ([Int] -> Int) -> [Int] -> [Int]
step f xs
    | allSame xs = [f xs]
    | otherwise = f xs : step f (zipWith (-) (tail xs) xs)

sol :: ([Int] -> Int) -> ([Int] -> Int) -> Text -> Int
sol f g = sum . map ((f . step g) . map read . words . Text.unpack) . Text.lines

solve :: AOC
solve = AOC 9 (sol sum last) (sol diffr head)
