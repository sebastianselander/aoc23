{-# LANGUAGE OverloadedLists #-}

module Day14 (solve) where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Lude
import Prelude hiding (round)

import Data.List.Extra (maximumOn)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P

data Direction = North | West | South | East

move
    :: Int
    -> Int
    -> HashMap (Int, Int) Char
    -> Direction
    -> (Int, Int)
    -> HashMap (Int, Int) Char
move width height hm dir idx =
    case hm HM.!? idx of
        Just 'O' ->
            case dir of
                North -> HM.insert (north idx) 'O' $ HM.delete idx hm
                West -> HM.insert (west idx) 'O' $ HM.delete idx hm
                South -> HM.insert (south idx) 'O' $ HM.delete idx hm
                East -> HM.insert (east idx) 'O' $ HM.delete idx hm
        _ -> hm
  where
    north ix@(x, y)
        | y > 0
        , Nothing <- hm HM.!? (x, y - 1) =
            north (x, y - 1)
        | otherwise = ix
    south ix@(x, y)
        | y < height
        , Nothing <- hm HM.!? (x, y + 1) =
            south (x, y + 1)
        | otherwise = ix
    west ix@(x, y)
        | x > 0
        , Nothing <- hm HM.!? (x - 1, y) =
            west (x - 1, y)
        | otherwise = ix
    east ix@(x, y)
        | x < width
        , Nothing <- hm HM.!? (x + 1, y) =
            east (x + 1, y)
        | otherwise = ix

moveAll
    :: Direction
    -> Int
    -> Int
    -> [(Int, Int)]
    -> HashMap (Int, Int) Char
    -> HashMap (Int, Int) Char
moveAll dir width height ixs m = foldl' (\hm -> move width height hm dir) m ixs

p1 :: Text -> Int
p1 t = sum . points $ moveAll North w h ixs m
  where
    (w, h, m) = parse t
    ixs = indices w h
    points m = [succ h - y | idx@(_, y) <- ixs, Just 'O' == m HM.!? idx]

runCycle
    :: Int -> Int -> HashMap (Int, Int) Char -> HashMap (Int, Int) Char
runCycle w h m =
    moveAll East w h (reverse ixs) $
        moveAll South w h (reverse ixs) $
            moveAll West w h ixs $
                moveAll North w h ixs m
  where
    ixs = indices w h

run
    :: Int -> Int -> HashMap (Int, Int) Char -> HashMap (Int, Int) Char
run w h = go 1_000_000_000 []
  where
    go
        :: Int
        -> HashMap (HashMap (Int, Int) Char) Int
        -> HashMap (Int, Int) Char
        -> HashMap (Int, Int) Char
    go n memo m
        | n < 1 = m
        | otherwise = case memo HM.!? m of
            Just n' | n > (n' - n) -> go (n `mod` (n' - n)) memo m
            _ -> let m' = runCycle w h m in go (n - 1) (HM.insert m n memo) m'

p2 :: Text -> Int
p2 t = sum $ points $ run w h m
  where
    (w, h, m) = parse t
    ixs = indices w h
    points m = [succ h - y | idx@(_, y) <- ixs, Just 'O' == m HM.!? idx]

indices :: (Num b, Num a, Enum b, Enum a) => b -> a -> [(a, b)]
indices w h = [(x, y) | y <- [0 .. w], x <- [0 .. h]]

solve :: AOC
solve = AOC 14 p1 p2

-- Parse

parse :: Text -> (Int, Int, HashMap (Int, Int) Char)
parse t = case P.parseMaybe (lineP `P.sepEndBy` P.newline) t of
    Nothing -> error "Failed parsing"
    Just x ->
        ( width $ concat x
        , height $ concat x
        , HM.fromList $ filter ((/= '.') . snd) $ concat x
        )
  where
    height = snd . fst . maximumOn (snd . fst)
    width = fst . fst . maximumOn (fst . fst)

lineP :: Parser [((Int, Int), Char)]
lineP = P.some $ round <|> cube <|> dot

round :: Parser ((Int, Int), Char)
round = P.char 'O' *> ((,'O') <$> pos)

cube :: Parser ((Int, Int), Char)
cube = P.char '#' *> ((,'#') <$> pos)

-- Meh
dot :: Parser ((Int, Int), Char)
dot = P.char '.' *> ((,'.') <$> pos)
