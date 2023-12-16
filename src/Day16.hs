module Day16 (solve) where

import Control.Monad.State (State, execState, get, put)
import Data.Matrix
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Lude hiding (Down, Left, Right)
import Prelude hiding (Left, Right)

data Tile = Empty | ForwardSlash | BackwardSlash | Dash | Pipe

data Direction = Up | Down | Left | Right
    deriving (Eq, Ord)

type Cache = Set (Direction, Int, Int)

convert :: Char -> Tile
convert '.' = Empty
convert '/' = ForwardSlash
convert '\\' = BackwardSlash
convert '-' = Dash
convert '|' = Pipe
convert _ = error "invalid char"

parse :: Text -> Matrix Tile
parse = fromLists . map (map convert . Text.unpack) . Text.lines

seen :: Cache -> Direction -> (Int, Int) -> Bool
seen cache dir (x, y) = Set.member (dir, x, y) cache

add :: Direction -> (Int, Int) -> Cache -> Cache
add dir (x, y) = Set.insert (dir, x, y)

stripDirection :: Cache -> Set (Int, Int)
stripDirection = Set.map (\(_, b, c) -> (b, c))

(.!) :: Matrix a -> (Int, Int) -> a
(.!) m (x, y) = m ! (y, x)

walk :: (Int, Int) -> Direction -> State (Matrix Tile, Cache) ()
walk (x, y) dir = do
    (grid, cache) <- get
    if
        | x > nrows grid || y > ncols grid || x < 1 || y < 1 -> pure ()
        | seen cache dir (x, y) -> pure ()
        | otherwise -> do
            put (grid, add dir (x, y) cache)
            case dir of
                Up -> case grid .! (x, y) of
                    ForwardSlash -> walk (x + 1, y) Right
                    BackwardSlash -> walk (x - 1, y) Left
                    Dash ->
                        (<>)
                            <$> walk (x + 1, y) Right
                            <*> walk (x - 1, y) Left
                    _ -> walk (x, y - 1) Up
                Down -> case grid .! (x, y) of
                    ForwardSlash -> walk (x - 1, y) Left
                    BackwardSlash -> walk (x + 1, y) Right
                    Dash ->
                        (<>)
                            <$> walk (x + 1, y) Right
                            <*> walk (x - 1, y) Left
                    _ -> walk (x, y + 1) Down
                Right -> case grid .! (x, y) of
                    ForwardSlash -> walk (x, y - 1) Up
                    BackwardSlash -> walk (x, y + 1) Down
                    Pipe ->
                        (<>)
                            <$> walk (x, y - 1) Up
                            <*> walk (x, y + 1) Down
                    _ -> walk (x + 1, y) Right
                Left -> case grid .! (x, y) of
                    ForwardSlash -> walk (x, y + 1) Down
                    BackwardSlash -> walk (x, y - 1) Up
                    Pipe ->
                        (<>)
                            <$> walk (x, y - 1) Up
                            <*> walk (x, y + 1) Down
                    _ -> walk (x - 1, y) Left

p1 :: Text -> Int
p1 =
    length
        . stripDirection
        . snd
        . execState (walk (1, 1) Right)
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

p2 :: Text -> Int
p2 t =
    maximum $
        map
            ( length
                . stripDirection
                . snd
                . flip execState (p, mempty)
                . \(a, b, c) -> walk (a, b) c
            )
            (starts p)
  where
    p = parse t

--
solve :: AOC
solve = AOC 16 p1 p2
