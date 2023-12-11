module Day11 (solve) where

import Data.List.Extra (allSame)
import Data.Text qualified as Text
import Lude

parse :: Text -> [String]
parse = lines . Text.unpack

type Index = (Int, Int)

indices :: [String] -> [Index]
indices xs =
    [ (x, y) | x <- [0 .. length (head xs) - 1]
    , y <- [0 .. length xs - 1]
    , xs !! y !! x == '#' ]

emptys :: [[Char]] -> ([Int], [Int])
emptys =
  elemIndices True . map allSame &&& elemIndices True . map allSame . transpose

expand :: Int -> Int -> [Index] -> [Int] -> [Int] -> [Index]
expand _ _ [] _ _ = []
expand n m ((x, y) : is) rows cols =
    (x + (n - 1) * r, y + (m - 1) * c) : expand n m is rows cols
  where
    r = length $ filter (< x) cols
    c = length $ filter (< y) rows

sol :: Int -> Int -> Text -> Int
sol n m t = (sum . concat . onOthers manhattan $ expand n m inds rows cols) `div` 2
  where
    (rows, cols) = emptys p
    inds = indices p
    p = parse t

p1 :: Text -> Int
p1 = sol 2 2

p2 :: Text -> Int
p2 = sol 1_000_000 1_000_000

solve :: AOC
solve = AOC 11 p1 p2
