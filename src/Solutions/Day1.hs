{-# LANGUAGE OverloadedStrings #-}

module Solutions.Day1 (solve) where

import Data.Char
import Lude
import Data.List (isPrefixOf)

p1 :: String -> Int
p1 = sum . map (f . map digitToInt . filter isDigit) . lines

p2 :: String -> Int
p2 = sum . map (f . ugly) . lines

f :: [Int] -> Int
f xs = head xs * 10 + last xs

ugly :: String -> [Int]
ugly [] = []
ugly s
  -- Tail instead of drop (length of prefix) :(
  | "one"   `isPrefixOf` s = 1 : ugly (tail s)
  | "two"   `isPrefixOf` s = 2 : ugly (tail s)
  | "three" `isPrefixOf` s = 3 : ugly (tail s)
  | "four"  `isPrefixOf` s = 4 : ugly (tail s)
  | "five"  `isPrefixOf` s = 5 : ugly (tail s)
  | "six"   `isPrefixOf` s = 6 : ugly (tail s)
  | "seven" `isPrefixOf` s = 7 : ugly (tail s)
  | "eight" `isPrefixOf` s = 8 : ugly (tail s)
  | "nine"  `isPrefixOf` s = 9 : ugly (tail s)
  | "1"     `isPrefixOf` s = 1 : ugly (tail s)
  | "2"     `isPrefixOf` s = 2 : ugly (tail s)
  | "3"     `isPrefixOf` s = 3 : ugly (tail s)
  | "4"     `isPrefixOf` s = 4 : ugly (tail s)
  | "5"     `isPrefixOf` s = 5 : ugly (tail s)
  | "6"     `isPrefixOf` s = 6 : ugly (tail s)
  | "7"     `isPrefixOf` s = 7 : ugly (tail s)
  | "8"     `isPrefixOf` s = 8 : ugly (tail s)
  | "9"     `isPrefixOf` s = 9 : ugly (tail s)
  | otherwise              =     ugly (tail s)

solve :: AOC
solve = AOC 1 p1 p2
