module Day18 (solve) where

import Data.List.Extra (dropEnd)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (pack, unpack)
import Lude
import Control.Monad.State

parse :: Text -> [(Char, Int, String)]
parse = map (clean . words) . lines . unpack
  where
    clean [a, b, c] = (head a, read b, dropEnd 1 $ tail c)

border :: (Int, Int) -> [(Char, Int, String)] -> Set (Int, Int)
border _ [] = Set.empty
border (row, col) ((char, step, _) : xs) =
    let (ix', ixs) = case char of
            'U' -> ((row - step, col), [(x, col) | x <- [row - 1, row - 2 .. row - step]])
            'D' -> ((row + step, col), [(x, col) | x <- [row + 1 .. row + step]])
            'L' -> ((row, col - step), [(row, y) | y <- [col - 1, col - 2 .. col - step]])
            'R' -> ((row, col + step), [(row, y) | y <- [col + 1 .. col + step]])
            _ -> error "unreachable!"
     in set ixs `Set.union` border ix' xs

flood :: (Int, Int) -> State (Set (Int, Int)) ()
flood (row, col)
   = do
      visited <- get
      put (Set.insert (row, col) visited)
      let ns = filter (`Set.notMember` visited) [(row, col + 1), (row, col - 1), (row + 1, col), (row - 1, col)]
      mapM_ flood  ns

p1 :: Text -> Int
p1 = Set.size . execState (flood (1,1)) . border (0, 0) . parse

solve :: AOC
solve = AOC 18 p1 id
