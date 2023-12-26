module Day16 (solve) where

import Control.Monad.State (State, execState, get, put)
import Data.Matrix (Matrix, fromLists, ncols, nrows, (!))
import Data.Set (Set)
import Data.Set qualified as Set
import Lude hiding (Down, Left, Right)
import Prelude hiding (Left, Right)
import Control.Parallel.Strategies (parMap, rpar)

data Tile = Empty | ForwardSlash | BackwardSlash | Dash | Pipe

data Direction = Up | Down | Left | Right
    deriving (Eq, Ord)

type Cache = Set (Direction, Int, Int)

convert :: Char -> Tile
convert = \case
    '.' -> Empty
    '/' -> ForwardSlash
    '\\' -> BackwardSlash
    '-' -> Dash
    '|' -> Pipe
    _ -> error "invalid char"

parse :: String -> Matrix Tile
parse = fromLists . map (map convert) . lines

seen :: Cache -> Direction -> (Int, Int) -> Bool
seen cache dir (x, y) = Set.member (dir, x, y) cache

add :: Direction -> (Int, Int) -> Cache -> Cache
add dir (x, y) = Set.insert (dir, x, y)

stripDirection :: Cache -> Set (Int, Int)
stripDirection = Set.map (\(_, b, c) -> (b, c))

(.!) :: Matrix a -> (Int, Int) -> a
(.!) m (x, y) = m ! (y, x)

walk :: (Int, Int, Direction) -> State (Matrix Tile, Cache) ()
walk (x, y, direction) = do
    (grid, cache) <- get
    if
        | x > nrows grid || y > ncols grid || x < 1 || y < 1 -> pure ()
        | seen cache direction (x, y) -> pure ()
        | otherwise -> do
            put (grid, add direction (x, y) cache)
            case direction of
                Up -> case grid .! (x, y) of
                    ForwardSlash -> walk right
                    BackwardSlash -> walk left
                    Dash -> (<>) <$> walk left <*> walk right
                    _ -> walk up
                Down -> case grid .! (x, y) of
                    ForwardSlash -> walk left
                    BackwardSlash -> walk right
                    Dash -> (<>) <$> walk left <*> walk right
                    _ -> walk down
                Right -> case grid .! (x, y) of
                    ForwardSlash -> walk up
                    BackwardSlash -> walk down
                    Pipe -> (<>) <$> walk up <*> walk down
                    _ -> walk right
                Left -> case grid .! (x, y) of
                    ForwardSlash -> walk down
                    BackwardSlash -> walk up
                    Pipe -> (<>) <$> walk up <*> walk down
                    _ -> walk left
  where
    up = newPos Up (x, y)
    down = newPos Down (x, y)
    left = newPos Left (x, y)
    right = newPos Right (x, y)
    newPos d (x, y) = case d of
        Up -> (x, y - 1, Up)
        Down -> (x, y + 1, Down)
        Right -> (x + 1, y, Right)
        Left -> (x - 1, y, Left)

p1 :: String -> Int
p1 =
    length
        . stripDirection
        . snd
        . execState (walk (1, 1, Right))
        . (,mempty)
        . parse

starts :: Matrix Tile -> [(Int, Int, Direction)]
starts m =
    zip3 [1 .. cols] (repeat 1) (repeat Down)
        <> zip3 [1 .. cols] (repeat rows) (repeat Up)
        <> zip3 (repeat 1) [1 .. rows] (repeat Right)
        <> zip3 (repeat cols) [1 .. rows] (repeat Left)
  where
    cols = ncols m
    rows = nrows m

p2 :: String -> Int
p2 t =
    maximum
        $ parMap rpar
            ( length
                . stripDirection
                . snd
                . flip execState (p, mempty)
                . walk
            )
        $ starts p
  where
    p = parse t

solve :: AOC
solve = AOC 16 p1 p2
