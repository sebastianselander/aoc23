{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Day10 (solve) where

import Algorithm.Search (bfsM)
import Control.Monad.State (
    State,
    get,
    put,
    runState,
 )
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Vector (Vector, (!?))
import Data.Vector qualified as Vec
import Lude

type Matrix a = Vector (Vector a)
type Index = (Int, Int)

parse :: Text -> Matrix Char
parse = Vec.fromList . map (Vec.fromList . Text.unpack) . Text.lines

sIndex :: Matrix Char -> Index
sIndex matrix = (x, y)
  where
    (x, y, _) =
        Vec.foldl'
            ( \(x, y, b) z ->
                case z of
                    Nothing -> bool (x, y + 1, b) (x, y, b) b
                    Just x' -> (x', y, True)
            )
            (0, 0, False)
            $ Vec.map (Vec.findIndex (== 'S')) matrix

sadj :: Matrix Char -> (Int, Int) -> [(Int, Int)]
sadj m (x, y) =
    bool [] [leftI] left
        ++ bool [] [rightI] right
        ++ bool [] [aboveI] above
        ++ bool [] [belowI] below
  where
    leftI = (x - 1, y)
    left = idx leftI m `elem` ([Just '-', Just 'L', Just 'F'] :: [Maybe Char])
    rightI = (x + 1, y)
    right = idx rightI m `elem` ([Just '-', Just 'J', Just '7'] :: [Maybe Char])
    belowI = (x, y + 1)
    below = idx belowI m `elem` ([Just 'L', Just 'J', Just '|'] :: [Maybe Char])
    aboveI = (x, y - 1)
    above = idx aboveI m `elem` ([Just '|', Just '7', Just 'F'] :: [Maybe Char])

idx :: Index -> Matrix Char -> Maybe Char
idx (x, y) m = do
    v <- m !? y
    v !? x

findLoop :: Index -> Matrix Char -> (Maybe [Index], Set Index)
findLoop startIndex matrix =
    runState (bfsM next done startIndex) mempty
  where
    done :: Index -> State (Set Index) Bool
    done i = do
        visited <- get
        case neighb i matrix of
            Just s ->
                if s `Set.isSubsetOf` visited
                    then put (Set.insert i visited) >> pure True
                    else pure False
            Nothing -> pure False

    next :: Index -> State (Set Index) (Set Index)
    next i = do
        visited <- get
        if i == startIndex
            then put visited
            else put (Set.insert i visited)
        pure $ fromMaybe [] (neighb i matrix)

    neighb :: Index -> Matrix Char -> Maybe (Set Index)
    neighb i@(x, y) m = case idx i m of
        Just '|' -> pure [(x, y - 1), (x, y + 1)]
        Just '-' -> pure [(x - 1, y), (x + 1, y)]
        Just 'L' -> pure [(x, y - 1), (x + 1, y)]
        Just 'J' -> pure [(x, y - 1), (x - 1, y)]
        Just '7' -> pure [(x, y + 1), (x - 1, y)]
        Just 'F' -> pure [(x, y + 1), (x + 1, y)]
        Just 'S' -> pure $ Set.fromList $ sadj m i
        _ -> Nothing

marked :: Matrix Char -> Set Index
marked m = Set.insert startIndex $ snd $ findLoop startIndex m
  where
    startIndex = sIndex m

p1 :: Text -> Int
p1 = length . fromJust . fst . uncurry findLoop . (sIndex &&& id) . parse

p2 :: Text -> Int
p2 t =
    sum
        . map (flip count 0 . fixW . wall p . sort)
        . groupBy ((==) `on` snd)
        . sortOn snd
        . Set.toList
        . marked
        $ p
  where
    p = parse t

data Wall = Wall
    { start :: Int
    , startChar :: Char
    , end :: Int
    , endChar :: Char
    , size :: Int
    }
    deriving (Show)

count :: [Wall] -> Int -> Int
count [] _ = 0
count [_] _ = 0
count (x : xs) size
    | odd (size + x.size) =
        pred ((head xs).start - x.end) + count xs (size + x.size)
    | otherwise = count xs (size + x.size)

fixW :: [Wall] -> [Wall]
fixW [] = []
fixW (x : xs)
    | x.startChar == 'L' && x.endChar == 'J'
        || x.startChar == 'F' && x.endChar == '7' =
        x {size = 2} : fixW xs
    | otherwise = x : fixW xs

wall :: Matrix Char -> [Index] -> [Wall]
wall _ [] = []
wall m (x : xs) = case idx x m of
    Just 'L' -> Wall (fst x) 'L' (fst i) c 1 : wall m ys
    Just 'F' -> Wall (fst x) 'F' (fst i) c 1 : wall m ys
    Just '|' -> Wall (fst x) '|' (fst x) '|' 1 : wall m xs
    Just '7' -> Wall (fst x) '7' (fst x) '7' 1 : wall m xs
    Just 'J' -> Wall (fst x) 'J' (fst x) 'J' 1 : wall m xs
    Just '-' -> error "should not happen"
    _ -> wall m xs
  where
    char = fromJust $ idx x m
    (c, i, ys) = findEnd x char xs
    findEnd :: Index -> Char -> [Index] -> (Char, Index, [Index])
    findEnd i c [] = (c, i, [])
    findEnd i c (x : xs) = case idx x m of
        Just '|' -> (c, i, x : xs)
        Just '-' -> findEnd x '-' xs
        Just 'L' -> (c, i, x : xs)
        Just 'J' -> ('J', x, xs)
        Just '7' -> ('7', x, xs)
        Just 'F' -> (c, i, x : xs)
        Just 'S' -> findEnd x 'S' xs
        _ -> undefined

solve :: AOC
solve = AOC 10 p1 p2
