module Day09 (solve) where

import Data.List.Extra
import Lude

step :: ([Int] -> Int) -> [Int] -> [Int]
step f xs
    | allSame xs = [f xs]
    | otherwise = f xs : step f (zipWith (-) (tail xs) xs)

sol :: ([Int] -> [Int]) -> String -> Int
sol f =
    sum
        . map ((foldr (-) 0 . step head . f) . map read . words)
        . lines

solve :: AOC
solve = AOC 9 (sol reverse) (sol id)
