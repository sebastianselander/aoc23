module Solutions.Day07 (solve) where

import Data.List.Extra
import Data.Text qualified as Text
import Lude

normal, joker :: String
normal = "23456789TJQKA"
joker = "J23456789TQKA"

parse :: Text -> [(String, Int)]
parse = map (second read . listToTuple . words . Text.unpack) . Text.lines

order :: String -> Int
order = go . sort . map length . group . sort
  where go = \case
          [1, 1, 1, 1, 1] -> 0
          [1, 1, 1, 2]    -> 1
          [1, 2, 2]       -> 2
          [1, 1, 3]       -> 3
          [2, 3]          -> 4
          [1, 4]          -> 5
          [5]             -> 6
          _               -> error "not a hand"

jokerOrder :: String -> Int
jokerOrder str =
    maximum $ map (order . \x -> replace "J" (singleton x) str) joker

compareHands :: String -> (String -> Int) -> String -> String -> Ordering
compareHands cr f as bs = case comparing f as bs of
    EQ -> go as bs
    x -> x
  where
    go :: String -> String -> Ordering
    go [] [] = EQ
    go (x : xs) (y : ys) =
        case comparing (fromJust . flip elemIndex cr) x y of
            LT -> LT
            GT -> GT
            EQ -> go xs ys

p1 :: Text -> Int
p1 = sum
   . zipWith (*) [1 ..]
   . map snd
   . sortBy (compareHands normal order `on` fst)
   . parse

p2 :: Text -> Int
p2 = sum
   . zipWith (*) [1 ..]
   . map snd
   . sortBy (compareHands joker jokerOrder `on` fst)
   . parse

solve :: AOC
solve = AOC 7 p1 p2
