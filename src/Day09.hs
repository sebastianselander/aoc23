module Day09 (solve) where

import Data.List.Extra
import Data.Text qualified as T (lines, unpack)
import Lude

step :: ([Int] -> Int) -> [Int] -> [Int]
step f xs
    | allSame xs = [f xs]
    | otherwise = f xs : step f (zipWith (-) (tail xs) xs)

sol :: ([Int] -> [Int]) -> Text -> Int
sol f =
    sum . map ((diffr . step head . f) . map read . words . T.unpack) . T.lines

solve :: AOC
solve = AOC 9 (sol reverse) (sol id)
