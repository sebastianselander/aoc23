{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Day10 (solve) where

import Algorithm.Search (bfsM)
import Control.Monad.State (
    State,
    evalState,
    execState,
    get,
    gets,
    put,
    runState,
 )
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Vector (Vector, (!?))
import Data.Vector qualified as Vec
import Debug.Trace (trace, traceShowM)
import Lude
import Data.List.Extra (chunksOf)

inp, test1, test2, test3, test4, test5, test6 :: Text
inp = Text.pack $ unsafePerformIO $ readFile "inputs/day10"
test1 = Text.pack $ unsafePerformIO $ readFile "test/day10_1"
test2 = Text.pack $ unsafePerformIO $ readFile "test/day10_2"
test3 = Text.pack $ unsafePerformIO $ readFile "test/day10_3"
test4 = Text.pack $ unsafePerformIO $ readFile "test/day10_4"
test5 = Text.pack $ unsafePerformIO $ readFile "test/day10_5"
test6 = Text.pack $ unsafePerformIO $ readFile "test/day10_last"

type Matrix a = Vector (Vector a)
type Index = (Int, Int) -- (x,y)
type St = (Set Index, Index)

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

enclosed t =
    -- sum
    --     . map (fixWalls p . sort)
        groupBy ((==) `on` snd)
        . sortOn snd
        . Set.toList
        . marked
        $ p
  where
    p = parse t

p2 :: Text -> Int
p2 = undefined

solve :: AOC
solve = AOC 10 p1 id
