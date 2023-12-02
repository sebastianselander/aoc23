module Solutions.Day2 where

import Control.Monad.State
import Data.Foldable (traverse_)
import Lude
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as L

data Color = Red | Green | Blue
    deriving (Show)

data Game = Game Int [[(Int, Color)]]
    deriving (Show)

pColor :: Parser (Int, Color)
pColor = do
    n <- L.decimal <* P.space
    color <- "blue" $> Blue <|> "red" $> Red <|> "green" $> Green
    pure (n, color)

pOnePick :: Parser [(Int, Color)]
pOnePick = pColor `P.sepBy` ", "

pGame :: Parser Game
pGame = do
    ident <- "Game " *> L.decimal <* ": "
    res <- pOnePick `P.sepBy` "; "
    pure $ Game ident res

parse :: Parser [Game]
parse = P.some (pGame <* "\n") <* P.eof

impossible :: Game -> Bool
impossible (Game _ games) = any (any f) games
  where
    f :: (Int, Color) -> Bool
    f (n, Red) = n > 12
    f (n, Green) = n > 13
    f (n, Blue) = n > 14

p1 :: Text -> Int
p1 = sum
   . map (\(Game n _) -> n)
   . filter (not . impossible)
   . fromJust
   . P.parseMaybe parse

data Minimal = M
    { redCount :: Int
    , blueCount :: Int
    , greenCount :: Int
    }
    deriving (Show)

gameToMini :: [(Int, Color)] -> State Minimal ()
gameToMini [] = pure ()
gameToMini (x : xs) = do
    m <- get
    case x of
        (n, Blue) -> do
            let b = max n m.blueCount
            put (M m.redCount b m.greenCount)
            gameToMini xs
        (n, Green) -> do
            let b = max n m.greenCount
            put (M m.redCount m.blueCount b)
            gameToMini xs
        (n, Red) -> do
            let b = max n m.redCount
            put (M b m.blueCount m.greenCount)
            gameToMini xs

minimals :: [[(Int, Color)]] -> State Minimal ()
minimals = traverse_ gameToMini

run :: Game -> Int
run (Game _ games) =
    let (M a b c) = execState (minimals games) (M 0 0 0)
     in a * b * c

p2 :: Text -> Int
p2 = sum . map run . fromJust . P.parseMaybe parse

solve :: AOC
solve = AOC 2 p1 p2
