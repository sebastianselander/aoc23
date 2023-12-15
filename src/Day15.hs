module Day15 (solve) where

import Data.List.Extra (splitOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text qualified as Text
import Lude

parse :: Text -> [String]
parse = splitOn "," . filter (/= '\n') . Text.unpack

hash :: String -> Int
hash = foldl' (\acc x -> ((acc + fromEnum x) * 17) `mod` 256) 0

p1 :: Text -> Int
p1 = sum . map hash . parse

hashmap :: Map Int [(String, Int)] -> String -> Map Int [(String, Int)]
hashmap m xs
    | '=' `elem` xs =
        let (l, r) = toTuple $ splitOn "=" xs
         in Map.insertWith update (hash l) [(l, read r)] m
    | otherwise =
        let (l, _) = toTuple $ splitOn "-" xs
         in Map.adjust (filter ((/= l) . fst)) (hash l) m
  where
    update :: [(String, Int)] -> [(String, Int)] -> [(String, Int)]
    update [(l, r)] ys
        | elemOn fst l ys = map (\(x, y) -> bool (x, y) (l, r) (x == l)) ys
        | otherwise = ys <> [(l, r)]
    update _ _ = error "unreachable!"

total :: Map Int [(String, Int)] -> [Int]
total m = map (go m) $ Map.keys m
  where
    go :: Map Int [(String, Int)] -> Int -> Int
    go m key = case Map.lookup key m of
        Nothing -> 0
        Just xs -> go2 (key + 1) 1 xs
      where
        go2 _ _ [] = 0
        go2 n m ((_, r) : xs) = n * m * r + go2 n (m + 1) xs

p2 :: Text -> Int
p2 = sum . total . foldl' hashmap mempty . parse

solve :: AOC
solve = AOC 15 p1 p2
