module Solutions.Day08 (solve) where

import Lude
import Data.List.Extra
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text qualified as Text
import Debug.Trace (traceShow)

parse :: Text -> (String, Map String String)
parse t = (l, Map.fromList $ concatMap lineToTup m)
  where
    [l, r] = splitOn "\n\n" $ Text.unpack t
    m = lines r
    lineToTup :: String -> [(String, String)]
    lineToTup [a,b,c,_,_,_,_,l1,l2,l3,_,_,r1,r2,r3,_]
      = [(['L',a,b,c], [l1,l2,l3]), (['R',a,b,c], [r1,r2,r3])]

run1 :: String -> Map String String -> String -> Int
run1 [] _ _ = undefined
run1 (lr:lrs) m str = go (lr : str)
  where
    go s | traceShow s False = undefined
    go s = case Map.lookup s m of
        Just "ZZZ" -> 1
        Just s' -> 1 + run1 lrs m s'
        Nothing -> error $ "String '" ++ str ++ "' not found"

p1 :: Text -> Int
p1 t = run1 (cycle l) r "AAA"
  where
    (l, r) = parse t

endWithA :: Map String String -> [String]
endWithA m = nub . map tail . filter (('A'==) . last) $ Map.keys m

endWithZ :: [Char] -> Bool
endWithZ = (=='Z') . last

stepsToZ :: String -> Map String String -> String -> Integer
stepsToZ [] _ _ = undefined
stepsToZ (lr:lrs) m str = case Map.lookup (lr : str) m of
    Nothing -> error "bug"
    Just s | endWithZ s -> 1
           | otherwise -> 1 + stepsToZ lrs m s

run :: String -> Map String String -> [Integer]
run lrs m = map (stepsToZ lrs m) $ endWithA m

p2 :: Text -> Integer
p2 t = foldr1 lcm $ run (cycle l) r
  where
    (l, r) = parse t

solve :: AOC
solve = AOC 8 p1 p2
