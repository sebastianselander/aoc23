module Day12 (solve) where

import Control.Monad.Memo
import Data.Bifunctor
import Data.List.Extra (splitOn)
import Lude

parse :: String -> [(String, [Int])]
parse = map (f . words) . lines
  where
    f [l, r] = (l, map read $ splitOn "," r)
    f _ = error "parse error"


fold :: [(String, [Int])] -> [(String, [Int])]
fold = map (bimap f g)
  where
    f = intercalate "?" . replicate 5
    g = concat . replicate 5

countMemo :: String -> [Int] -> Memo (String, [Int]) Int Int
countMemo ss []
  | '#' `notElem` ss = pure 1
  | otherwise = pure 0
countMemo [] _ = pure 0
countMemo zz@(s : ss) (x : xs) = case s of
    '.' -> countMemo ss (x : xs)
    '#' -> do
        let (keep, ignore) = splitAt x zz
        bool
            (pure 0)
            (countMemo (safeTail ignore) xs)
            (fit keep x && isOk ignore)
    '?' -> do
        c1 <- for2 memo countMemo ('#' : ss) (x : xs)
        c2 <- for2 memo countMemo ('.' : ss) (x : xs)
        pure (c1 + c2)
    _ -> error "unknown char"
  where
    isOk ('#' : _) = False
    isOk _ = True
    fit ww n = '.' `notElem` ww && length ww == n

p1 :: String -> Int
p1 = sum . startEvalMemo . mapM (uncurry countMemo) . parse

p2 :: String -> Int
p2 = sum . startEvalMemo . mapM (uncurry countMemo) . fold . parse

solve :: AOC
solve = AOC 12 p1 p2
