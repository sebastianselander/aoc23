module Day02 where

import Lude
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as L

data Color = Red | Green | Blue
    deriving (Show, Enum)

data Game = Game
    { ident :: Int
    , games :: [[(Int, Color)]]
    }
    deriving (Show)

parse :: Parser [Game]
parse =
    P.some
        ( Game
            <$> ("Game " *> L.decimal <* ": ")
            <*> ( (,)
                    <$> (L.decimal <* P.space)
                    <*> ( "blue"
                            $> Blue
                            <|> "red"
                            $> Red
                            <|> "green"
                            $> Green
                        )
                )
                `P.sepBy` ", "
                `P.sepBy` "; "
            <* "\n"
        )
        <* P.eof

p1 :: String -> Int
p1 =
    sum
        . map (\(Game n _) -> n)
        . filter (not . any (any (\(n, c) -> n > 12 + fromEnum c)) . games)
        . fromJust
        . P.parseMaybe parse

p2 :: String -> Int
p2 =
    sum
        . map
            ( \xs ->
                maximum
                    [ r * g * b
                    | (r, Red) <- concat xs.games
                    , (g, Blue) <- concat xs.games
                    , (b, Green) <- concat xs.games
                    ]
            )
        . fromJust
        . P.parseMaybe parse

solve :: AOC
solve = AOC 2 p1 p2
