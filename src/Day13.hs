module Day13 (solve) where

import Data.List.Extra (splitOn, (!?))
import Lude hiding ((!?))

parse :: String -> [[String]]
parse = map lines . splitOn "\n\n"

reflect :: Int -> [String] -> Int
reflect stop m = go (length m - 1)
  where
    go 0 = 0
    go n
        | countElem False (concat $ zipWith (zipWith (==)) l r) == stop = n
        | otherwise = go (n - 1)
      where
        (l, r) = both (mapMaybe ((m !?) . pred)) $ unzip (along n)

along :: Int -> [(Int, Int)]
along n = zip [n, pred n .. 0] [succ n ..]

points :: ([String] -> Int) -> [String] -> Int
points f = uncurry (+) . ((100*) . f &&& f . transpose)

p1 :: String -> Int
p1 = sum . map (points (reflect 0)) . parse

p2 :: String -> Int
p2 = sum . map (points (reflect 1)) . parse

solve :: AOC
solve = AOC 13 p1 p2
