{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day25 (solve) where

import Data.List.Extra
import Data.Map (Map)
import Data.Map qualified as Map
import Lude

inp :: String
{-# NOINLINE inp #-}
inp = unsafePerformIO $ readFile "inputs/day25"

parse :: String -> Map String [String]
parse = foldr (add . (\[l, r] -> (l, words r)) . splitOn ": ") mempty . lines
  where
    add (l, xs) m =
        foldr (\x -> Map.insertWith (++) x [l]) (Map.insertWith (++) l xs m) xs

solve :: AOC
solve = AOC 25 id id
