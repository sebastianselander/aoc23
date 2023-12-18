module Day18 (solve) where

import Data.Bifunctor (bimap)
import Data.List.Extra (dropEnd)
import Data.Text (unpack)
import Lude
import Numeric

parse :: Text -> [(Char, Int, String)]
parse = map (clean . words) . lines . unpack
  where
    clean [a, b, c] = (head a, read b, drop 2 $ dropEnd 1 c)

border :: Bool -> [(Char, Int, String)] -> (Int, [(Int, Int)])
border b xs = go b (0, 0) xs
  where
    go _ _ [] = (0, [])
    go b (row, col) ((char', step', hex) : xs) =
        let step =
                if b
                    then fst $ head $ readHex (init hex)
                    else step'
            char =
                if b
                    then digToChar $ last hex
                    else char'
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
area (perim, xs) = (abs (go xs) + perim) `div` 2
  where
    (xl, yl) = head xs
    go [] = 0
    go [(xn, yn)] = xn * yl - yn * xl
    go ((x1, y1) : (x2, y2) : xs) = (x1 * y2 - y1 * x2) + go ((x2, y2) : xs)

p1 :: Text -> Int
p1 = succ . area . border False . parse

p2 :: Text -> Int
p2 = succ . area . border True . parse

solve :: AOC
solve = AOC 18 p1 p2
