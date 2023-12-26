{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day24 (solve) where

import Lude

data Hailstone = H
    { x :: Int
    , y :: Int
    , z :: Int
    , dx :: Int
    , dy :: Int
    , dz :: Int
    }
    deriving (Show)

data Line2D = L {a :: Int, b :: Int, c :: Int}
    deriving Show

parse :: String -> [Hailstone]
parse = map (hail . map (filter (/= ',')) . words) . lines
  where
    hail [x, y, z, _, dx, dy, dz] =
        H (read x) (read y) (read z) (read dx) (read dy) (read dz)

solve :: AOC
solve = AOC 24 id id

intersect :: Line2D -> Line2D -> (Int, Int)
intersect l1 l2 = todo
