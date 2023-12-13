module Day08 (solve) where

import Data.List.Extra
import Data.Map (Map, (!))
import Data.Map qualified as Map
import Data.Text qualified as Text
import Lude

parse :: Text -> (String, Map String String)
parse t = (l, Map.fromList $ concatMap lineToTup $ lines r)
  where
    [l, r] = splitOn "\n\n" $ Text.unpack t
    lineToTup :: String -> [(String, String)]
    lineToTup [a, b, c, _, _, _, _, l1, l2, l3, _, _, r1, r2, r3, _] =
        [(['L', a, b, c], [l1, l2, l3]), (['R', a, b, c], [r1, r2, r3])]

run1 :: String -> Map String String -> String -> Int
run1 (lr : lrs) m str = case m ! (lr : str) of
    "ZZZ" -> 1
    s' -> 1 + run1 lrs m s'

endWithA :: Map String String -> [String]
endWithA = nub . map tail . filter (('A' ==) . last) . Map.keys

endWithZ :: [Char] -> Bool
endWithZ = (== 'Z') . last

stepsToZ :: String -> Map String String -> String -> Int
stepsToZ (lr : lrs) m str
    | endWithZ s = 1
    | otherwise = 1 + stepsToZ lrs m s
  where
    s = m Map.! (lr : str)

p1 :: Text -> Int
p1 t = run1 (cycle l) r "AAA"
  where
    (l, r) = parse t

p2 :: Text -> Int
p2 t = foldl1' lcm $ map (stepsToZ (cycle l) r) $ endWithA r
  where
    (l, r) = parse t

solve :: AOC
solve = AOC 8 p1 p2
