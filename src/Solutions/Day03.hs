module Solutions.Day03 where

import Lude

import Data.List.Extra
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P

parse :: Text -> [String]
parse = map Text.unpack . Text.lines

adj :: (Int, Int) -> [(Int, Int)]
adj (x, y) =
    [ (x + 1, y + 1)
    , (x + 1, y - 1)
    , (x - 1, y + 1)
    , (x - 1, y - 1)
    , (x, y - 1)
    , (x, y + 1)
    , (x + 1, y)
    , (x - 1, y)
    ]

look :: [[a]] -> (Int, Int) -> Maybe a
look xs (x, y) = xs !? y >>= (!? x)

keep :: [String] -> (Int, Int) -> Maybe (Bool, Char)
keep matrix (x, y) = do
    let kept =
            any
                (\z -> (z /= '.') && (not . isDigit) z)
                (mapMaybe (look matrix) (adj (x, y)))
    case look matrix (x, y) of
        Just c -> pure (kept, c)
        Nothing -> Nothing

indices :: Int -> Int -> [(Int, Int)]
indices width height = [(x, y) | y <- [0 .. height - 1], x <- [0 .. width - 1]]

-- Cringe
clump :: [(Bool, Char)] -> [(Bool, Int)]
clump (a : b : c : xs)
    | isDigit (snd a) && isDigit (snd b) && isDigit (snd c) =
        (fst a || fst b || fst c, read (snd a : snd b : [snd c])) : clump xs
clump (a : b : xs)
    | isDigit (snd a) && isDigit (snd b) =
        (fst a || fst b, read (snd a : [snd b])) : clump xs
clump (a : xs)
    | isDigit (snd a) = (fst a, read [snd a]) : clump xs
    | otherwise = clump xs
clump _ = []

p1 :: Text -> Int
p1 x =
    sum
        . map snd
        . filter fst
        . clump
        $ mapMaybe (keep parsed) (indices width height)
  where
    parsed = parse x
    width = head (map length parsed)
    height = length parsed

data Rep = Ignore | Star | Number Int | NumberIndex (Int, Int)
    deriving (Show)

type IRep = (Int, Int, Rep)

isNumber :: Rep -> Bool
isNumber (Number _) = True
isNumber _ = False

clumpRep :: Int -> Int -> [Rep] -> [Rep]
clumpRep x y (Number a : Number b : Number c : xs) =
    NumberIndex (x + 2, y)
        : NumberIndex (x + 2, y)
        : Number (a * 100 + b * 10 + c)
        : clumpRep (x + 3) y xs
clumpRep x y (Number a : Number b : as) =
    NumberIndex (x + 1, y)
        : Number (a * 10 + b)
        : clumpRep (x + 2) y as
clumpRep x y (a : as) = a : clumpRep (x + 1) y as
clumpRep _ _ [] = []

parse2 :: Parser [Rep]
parse2 =
    P.some
        ( (Number . digitToInt <$> P.digitChar)
            <|> P.char '*'
            $> Star
            <|> P.asciiChar
            $> Ignore
        )

runParse2 :: Text -> [[IRep]]
runParse2 =
    indexify 0
        . adjust 0
        . map (fromJust . P.parseMaybe parse2)
        . Text.lines
  where
    adjust :: Int -> [[Rep]] -> [[Rep]]
    adjust y (x : xs) = clumpRep 0 y x : adjust (y + 1) xs
    adjust _ [] = []
    indexify :: Int -> [[a]] -> [[(Int, Int, a)]]
    indexify _ [] = []
    indexify y (x : xs) = zip3 [0 ..] (repeat y) x : indexify (succ y) xs

adjs :: [[IRep]] -> (Int, Int) -> [IRep]
adjs matrix idx = mapMaybe (look matrix) (adj idx)

toNum :: [[IRep]] -> Set (Int, Int) -> [IRep] -> [Int]
toNum _ _ [] = []
toNum matrix s (a : as) = case a of
    (x, y, Number n)
        | (x, y) `Set.notMember` s ->
            n : toNum matrix (Set.insert (x, y) s) as
    (_, _, NumberIndex (x', y')) | (x', y') `Set.notMember` s ->
        case look matrix (x', y') of
            Just (_, _, Number n) -> n : toNum matrix (Set.insert (x', y') s) as
            _ -> error "fail"
    _ -> toNum matrix s as

adjStar :: [[IRep]] -> (Int, Int) -> Int
adjStar matrix (x, y) = do
    case look matrix (x, y) of
        Just (_, _, Star) ->
            let adjacents = adjs matrix (x, y)
                xs = toNum matrix mempty adjacents
             in fromEnum (length xs == 2) * product xs
        _ -> 0

p2 :: Text -> Int
p2 s = sum $ map (adjStar parsed) (indices width height)
  where
    parsed = runParse2 s
    width = head (map length parsed)
    height = length parsed

solve :: AOC
solve = AOC 3 p1 p2
