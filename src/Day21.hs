module Day21 (solve) where

import Control.Monad.State
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Matrix hiding (trace)
import Data.Vector qualified as Vec
import Lude hiding ((<|>))

t = parse test
ts = fromJust $ startPos t
tinp = parse (unsafePerformIO $ readFile "inputs/day21")

type Coord = (Int, Int)

parse :: String -> Matrix Char
parse = fromLists . lines

startPos :: Matrix Char -> Maybe Coord
startPos m = second succ <$> go (nrows m) m
  where
    go :: Int -> Matrix Char -> Maybe (Int, Int)
    go (-1) _ = Nothing
    go n mat = do
        vec <- safeGetRow n mat
        case Vec.elemIndex 'S' vec of
          Nothing -> go (n - 1) mat
          Just i -> Just (n, i)

reachable :: Int -> Matrix Char -> Map Coord Int
reachable n matrix = execState (go [(1, start)]) (Map.singleton start 0)
  where
    start = fromJust $ startPos matrix
    go :: [(Int, Coord)] -> State (Map Coord Int) ()
    go [] = pure ()
    go ((m, coord) : xs)
        | n + 1 == m = pure ()
        | otherwise = do
            visited <- get
            let visit =
                    [ coord'
                    | coord' <-
                        [ above coord
                        , left coord
                        , below coord
                        , right coord
                        ]
                    , Just '.' == matrix !? coord'
                    , Map.notMember coord' visited
                    ]
            put (foldr (`Map.insert` m) visited visit)
            go (xs ++ [ (m + 1,v) | v <- visit])

triple :: Matrix Char -> Matrix Char
triple m = (m' <|> m' <|> m')
       <-> (m' <|> m <|> m')
       <-> (m' <|> m' <|> m')
  where
    start = fromJust (startPos m)
    m'    = setElem '.' start m

replaceWithDist :: Int -> Matrix Char -> Matrix Char
replaceWithDist n m = foldr replace m (Map.toList (reachable n m))
  where
    replace (coord, n) = setElem (toHex n) coord

toHex :: Int -> Char
toHex n
         = case n of
            0  -> 'S'
            1  -> '1'
            2  -> '2'
            3  -> '3'
            4  -> '4'
            5  -> '5'
            6  -> '6'
            7  -> '7'
            8  -> '8'
            9  -> '9'
            10 -> 'A'
            11 -> 'B'
            12 -> 'C'
            13 -> 'D'
            14 -> 'E'
            15 -> 'F'
            16 -> 'G'
            17 -> 'H'
            18 -> 'I'
            19 -> 'J'
            20 -> 'K'
            21 -> 'L'
            22 -> 'M'
            23 -> 'N'
            24 -> 'O'
            25 -> 'P'
            26 -> 'Q'
            27 -> 'R'
            28 -> 'S'
            29 -> 'T'
            30 -> 'U'
            31 -> 'V'
            32 -> 'W'
            33 -> 'X'
            34 -> 'Y'
            35 -> 'Z'
            36 -> 'a'
            37 -> 'b'
            38 -> 'c'
            39 -> 'd'
            40 -> 'e'
            41 -> 'f'
            42 -> 'g'
            43 -> 'h'
            44 -> 'i'
            45 -> 'j'
            46 -> 'k'
            47 -> 'l'
            48 -> 'm'
            49 -> 'n'
            50 -> 'o'
            51 -> 'p'
            52 -> 'q'
            53 -> 'r'
            54 -> 's'
            55 -> 't'
            56 -> 'u'
            57 -> 'v'
            58 -> 'w'
            59 -> 'x'
            60 -> 'y'
            61 -> 'z'
            62 -> 'å'
            63 -> 'ä'
            64 -> 'ö'
            n  -> toEnum n

paint :: Matrix Char -> String
paint = unlines . toLists

p1 :: String -> Int
p1 = length . filter even . Map.elems . reachable 64 . parse

p2 :: String -> Int
p2 = length . Map.elems . reachable maxBound . parse

solve :: AOC
solve = AOC 21 p1 p2
