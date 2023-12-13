module Day13 (solve) where

import Data.Text (unpack)
import Lude
import Data.List.Extra ((!?), splitOn)

parse :: Text -> [[[Char]]]
parse = map lines . splitOn "\n\n" . unpack

reflect :: [[Char]] -> Maybe Int
reflect m = go (length m - 1)
  where
    go 0 = Nothing
    go n | all and (zipWith (zipWith (==)) l r) = Just n
         | otherwise = go (n - 1)
      where
        (l, r) = both (mapMaybe (\x -> m !? (x - 1))) $ unzip zipped
        zipped = along n (n + 1)

along :: Int -> Int -> [(Int, Int)]
along 0 m = [(0,m)]
along n m = (n,m) : along (n - 1) (m + 1)

run :: [[Char]] -> Int
run m | res == 0 = 0
      | otherwise = res
  where
    res = fromMaybe 0 x * 100 + fromMaybe 0 y
    x = reflect m
    y = reflect (transpose m)

p1 :: Text -> Int
p1 = sum . map run . parse

p2 :: Text -> Int
p2 = undefined

solve :: AOC
solve = AOC 13 p1 p2
