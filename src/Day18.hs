module Day18 (solve) where

import Data.Bifunctor (bimap)
import Data.List.Extra (dropEnd)
import Lude
import Numeric (readHex)

parse :: String -> [(Char, Int, String)]
parse = map (match . words) . lines
  where
    match [a, b, c] = (head a, read b, drop 2 $ dropEnd 1 c)
    match _ = error "invalid match"

border :: Bool -> [(Char, Int, String)] -> (Int, [(Int, Int)])
border b xs = go b (0, 0) xs
  where
    go _ _ [] = (0, [])
    go b (row, col) ((char', step', hex) : xs) =
        let (char, step) =
                if b
                    then
                        ( digToChar $ last hex
                        , fst $ head $ readHex (init hex)
                        )
                    else (char', step')
         in let ix = case char of
                    'U' -> (row - step, col)
                    'D' -> (row + step, col)
                    'L' -> (row, col - step)
                    'R' -> (row, col + step)
                    _ -> error "unreachable!"
             in bimap (+ step) (ix :) (go b ix xs)
      where
        digToChar '0' = 'R'
        digToChar '1' = 'D'
        digToChar '2' = 'L'
        digToChar '3' = 'U'
        digToChar _ = error "invalid char"

area :: (Int, [(Int, Int)]) -> Int
area (perim, xs) = succ ((abs (go xs) + perim) `div` 2)
  where
    (xl, yl) = head xs
    go [] = 0
    go [(xn, yn)] = xn * yl - yn * xl
    go ((x1, y1) : (x2, y2) : xs) = (x1 * y2 - y1 * x2) + go ((x2, y2) : xs)

p1 :: String -> Int
p1 = area . border False . parse

p2 :: String -> Int
p2 = area . border True . parse

solve :: AOC
solve = AOC 18 p1 p2
