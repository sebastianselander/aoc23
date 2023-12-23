module Day22 (solve) where

import Data.List.Extra (splitOn)
import Lude
import Control.Monad.State

data Cube = Cube
    { x :: (Int, Int)
    , y :: (Int, Int)
    , z :: (Int, Int)
    }
    deriving (Eq, Ord, Show)

parse :: String -> [Cube]
parse = sortOn z . map f . lines
  where
    f s =
        let [[x1, y1, z1], [x2, y2, z2]] = map (splitOn ",") (splitOn "~" s)
         in Cube (read x1, read x2) (read y1, read y2) (read z1, read z2)

moveDown :: Cube -> Cube
moveDown cube
    | inRange cube.z 1 = cube
    | otherwise = cube {z = both pred cube.z}

intersects :: Cube -> Cube -> Bool
intersects
    (Cube (xlow, xhigh) (ylow, yhigh) (zlow, zhigh))
    (Cube (xlow', xhigh') (ylow', yhigh') (zlow', zhigh')) =
        xlow <= xhigh'
            && xhigh >= xlow'
            && ylow <= yhigh'
            && yhigh >= ylow'
            && zlow <= zhigh'
            && zhigh >= zlow'

fall :: [Cube] -> State Int [Cube]
fall = fmap reverse . go []
  where
    go :: [Cube] -> [Cube] -> State Int [Cube]
    go xs [] = pure xs
    go xs (y : ys) =
        let down = top xs
            (l, r) = y.z
            change = min (l - down - 1) (r - down - 1)
            z' = (l - change, r - change)
         in do
             go (move (y {z = z'}) xs : xs) ys
      where
        top :: [Cube] -> Int
        top [] = 0
        top (x : xs) = max (uncurry max x.z) (top xs)

move :: Cube -> [Cube] -> Cube
move c cs
    | any (intersects c) cs = c
    | otherwise =
        if any (intersects (moveDown c)) cs || moveDown c == c
            then c
            else move (moveDown c) cs

falls :: [Cube] -> Int
falls = go []
  where
    go _ [] = 0
    go xs (y:ys) 
      | move y xs == y = go (y : xs) ys
      | otherwise = 1 + go (move y xs : xs) ys

disintegrate :: [Cube] -> State Int Int
disintegrate xs = sum <$> mapM (go xs) [0 .. length xs - 1]
  where
    go :: [Cube] -> Int -> State Int Int
    go xs n = let (l, r) = splitAt n xs
                  stack = l ++ tail r
               in do
                   st <- fall stack
                   modify (+falls stack)
                   if stack == st then pure 1 else pure 0

-- Slow as fuck, cba fixing
p1 :: String -> Int
p1 s = evalState (fall (parse s) >>= disintegrate) undefined

-- Slow, cba fixing
p2 :: String -> Int
p2 s = execState (fall (parse s) >>= disintegrate) 0

solve :: AOC
solve = AOC 22 p1 p2
