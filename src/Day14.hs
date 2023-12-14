module Day14 (solve) where

import Data.Text qualified as Text
import Data.Vector qualified as Vec
import Lude

parse :: Text -> Matrix Char
parse = Vec.fromList . map Vec.fromList . lines . Text.unpack

test :: Text
test = Text.pack $ unsafePerformIO $ readFile "test/test"

move :: (Int, Int) -> Matrix Char -> Matrix Char
move idx@(row, col) matrix = case matrix !!? idx of
    Just 'O' -> update (update matrix idx '.') idx' 'O'
    _ -> matrix
  where
    idx' = bool (row, minimum d) (row, col) (null d)
    d = dist (col - 1)
    dist c = case matrix !!? (row, c) of
        Just '.' -> c : dist (c - 1)
        _ -> []

run :: Matrix Char -> Int
run m = points $ foldl' (flip move) m indices
  where
    points m = sum [Vec.length m - y | y <- [0 .. Vec.length m - 1], x <- [0 .. Vec.length (m Vec.! 0) - 1], Just 'O' == m !!? (x,y) ]
    indices =
        [(x, y) | y <- [0 .. Vec.length m - 1], x <- [0 .. Vec.length (m Vec.! 0) - 1]]

p1 :: Text -> Int
p1 = run . parse

solve :: AOC
solve = AOC 14 p1 id
