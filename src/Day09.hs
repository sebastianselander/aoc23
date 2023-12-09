module Day09 (solve) where

import Data.List.Extra
import Data.Text qualified as Text
import Lude

step :: ([Int] -> Int) -> [Int] -> [Int]
step f xs
    | allSame xs = [f xs]
    | otherwise = f xs : step f (zipWith (-) (tail xs) xs)

p1 :: Text -> Int
p1 = sum . map ((sum . step last) . map read . words . Text.unpack) . Text.lines

p2 :: Text -> Int
p2 =
    sum
        . map ((foldr (-) 0 . step head) . map read . words . Text.unpack)
        . Text.lines

solve :: AOC
solve = AOC 9 p1 p2
