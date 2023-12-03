module Solutions.Day3 where

import Lude

import Data.Text qualified as Text
import Control.Monad.Reader
import Data.List.Extra
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Data.Map (Map)
import Data.Set (Set)
import Data.Set qualified as Set

parse :: Text -> [String]
parse = map Text.unpack . Text.lines

adj :: (Int, Int) -> [(Int,Int)]
adj (x,y) = [ (x+1, y+1)
            , (x+1, y-1)
            , (x-1, y+1)
            , (x-1, y-1)
            , (x, y-1)
            , (x, y+1)
            , (x+1, y)
            , (x-1, y)]

look :: [[a]] -> (Int, Int) -> Maybe a
look xs (x,y) = do
    ls <- xs !? y
    ls !? x

keep :: (Int, Int) -> Reader [String] (Maybe (Bool, Char))
keep (x,y) = do
    matrix <- ask
    let kept = any (\z -> (z /= '.') && (not . isDigit) z)
                   (mapMaybe (look matrix) (adj (x,y)))
    case look matrix (x,y) of
        Just c -> pure $ pure (kept, c)
        Nothing -> pure Nothing

indices :: Int -> Int -> [(Int, Int)]
indices width height = [(x,y) | y <- [0 .. height - 1], x <- [0 .. width - 1]]

clump :: [(Bool,Char)] -> [(Bool, Int)]
clump (a:b:c:xs)
  | isDigit (snd a) && isDigit (snd b) && isDigit (snd c) 
    = (fst a || fst b || fst c , read (snd a : snd b : [snd c])) : clump xs
clump (a:b:xs)
  | isDigit (snd a) && isDigit (snd b) 
    = (fst a || fst b , read (snd a : [snd b])) : clump xs
clump (a:xs)
  | isDigit (snd a) = (fst a, read [snd a]) : clump xs
  | otherwise       = clump xs
clump _             = []

p1' :: Text -> [(Bool, Int)]
p1' x = let parsed = parse x
            width = head (map length parsed)
            height = length parsed
            inter = catMaybes <$> mapM keep (indices width height)
        in clump $ runReader inter parsed

p1 :: Text -> Int
p1 = sum . map snd . filter fst . p1'

data Rep = Ignore | Star | Number Int | NumberIndex (Int, Int)
  deriving Show

type IRep = (Int, Int, Rep)

isNumber :: Rep -> Bool
isNumber (Number _) = True
isNumber _          = False

clumpRep :: Int -> Int -> [Rep] -> [Rep]
clumpRep x y (Number a:Number b:Number c:xs)
    = NumberIndex (x+2,y)
    : NumberIndex (x+2,y)
    : Number (a * 100 + b * 10 + c)
    : clumpRep (x + 3) y xs
clumpRep x y (Number a:Number b:as)
    = NumberIndex (x+1,y)
    : Number (a * 10 + b)
    : clumpRep (x + 2) y as
clumpRep x y (a:as) = a : clumpRep (x + 1) y as
clumpRep x y [] = []

parse2 :: Parser [Rep]
parse2 = P.some ((Number . digitToInt <$> P.digitChar)
            <|> P.char '*' $> Star
            <|> P.asciiChar $> Ignore)

runParse2 :: Text -> [[IRep]]
runParse2
    = indexify 0
    . adjust 0
    . map (fromJust . P.parseMaybe parse2)
    . Text.lines
  where
    adjust :: Int -> [[Rep]] -> [[Rep]]
    adjust y (x:xs) = clumpRep 0 y x : adjust (y + 1) xs
    adjust _ [] = []
    indexify :: Int -> [[a]] -> [[(Int, Int, a)]]
    indexify _ [] = []
    indexify y (x:xs) = zip3 [0 ..] (repeat y) x : indexify (succ y) xs

adjsAbove :: [[IRep]] -> (Int, Int) -> [IRep]
adjsAbove matrix (x,y) = mapMaybe (look matrix) [(x-1,y-1), (x,y-1), (x+1,y-1)]

adjsBelow :: [[IRep]] -> (Int, Int) -> [IRep]
adjsBelow matrix (x,y) = mapMaybe (look matrix) [(x-1,y+1), (x,y+1), (x+1,y+1)]

adjsLeft :: [[IRep]] -> (Int, Int) -> [IRep]
adjsLeft matrix (x,y) = mapMaybe (look matrix) [(x-1,y)]

adjsRight :: [[IRep]] -> (Int, Int) -> [IRep]
adjsRight matrix (x,y) = mapMaybe (look matrix) [(x+1,y)]

adjs :: [[IRep]] -> (Int, Int) -> [IRep]
adjs a b = adjsAbove a b ++ adjsLeft a b ++ adjsBelow a b ++ adjsRight a b

type StarMap = Map (Int, Int) [Int]

toNum :: [[IRep]] -> Set (Int, Int) -> [IRep] -> [Int]
toNum _ _ [] = []
toNum matrix s (a:as) = case a of
   (x,y,Number n) | (x,y) `Set.notMember` s 
        -> n : toNum matrix (Set.insert (x,y) s) as
   (_,_,NumberIndex (x',y')) | (x',y') `Set.notMember` s 
        -> case look matrix (x',y') of
        Just (_,_,Number n) -> n : toNum matrix (Set.insert (x',y') s) as
        _ -> error "fail"
   _ -> toNum matrix s as

adjStar :: [[IRep]] -> (Int, Int) -> Int
adjStar matrix (x,y) = do
    case look matrix (x,y) of
        Just (_,_,Star) -> let adjacents = adjs matrix (x,y)
                               xs = toNum matrix mempty adjacents
                            in if length xs == 2 then product xs
                            else 0
        _ -> 0

p2 :: Text -> Int
p2 s = let parsed = runParse2 s
           width = head (map length parsed)
           height = length parsed
        in sum $ map (adjStar parsed) (indices width height)

solve :: AOC
solve = AOC 3 p1 p2
