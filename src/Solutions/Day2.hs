module Solutions.Day2 where

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
    color <-
        "blue"
            $> Blue
            <|> "red"
            $> Red
            <|> "green"
            $> Green
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

maxes :: Game -> Int
maxes (Game _ xs)
    = maximum [n | (n, Red) <- concat xs]
    * maximum [n | (n, Green) <- concat xs]
    * maximum [n | (n, Blue) <- concat xs]

p2 :: Text -> Int
p2 = sum . map maxes . fromJust . P.parseMaybe parse

solve :: AOC
solve = AOC 2 p1 p2
