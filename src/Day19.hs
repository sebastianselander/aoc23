module Day19 (solve) where

import Data.IntSet (IntSet)
import Data.IntSet qualified as Set
import Data.List.Extra (splitOn)
import Lude
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as L

data Workflow = WF { name :: String , branches :: [Branch] , els :: String }
    deriving (Eq, Ord, Show)

data Branch = B
    { category :: Category
    , condition :: Condition
    , size :: Int
    , jump :: String
    }
    deriving (Eq, Ord, Show)

data Category = X | M | A | S
    deriving (Eq, Ord, Show)

data Condition
    = LessThan
    | GreaterThan
    deriving (Eq, Ord, Show)

type Rating = (Category, Int)

lookupWF :: String -> [Workflow] -> Maybe Workflow
lookupWF _ [] = Nothing
lookupWF s (x : xs)
    | x.name == s = Just x
    | otherwise = lookupWF s xs

runRating :: Workflow -> [Rating] -> String
runRating (WF _ [] e) _ = e
runRating (WF nm (b : brs) e) rs = case go b rs of
    Just s -> s
    Nothing -> runRating (WF nm brs e) rs
  where
    go :: Branch -> [Rating] -> Maybe String
    go _ [] = Nothing
    go (B cat cond sz jmp) ((cat', n) : rs)
        | cat == cat' && fromCond cond n sz = Just jmp
        | otherwise = go (B cat cond sz jmp) rs
      where
        fromCond :: Condition -> (Int -> Int -> Bool)
        fromCond LessThan = (<)
        fromCond GreaterThan = (>)

accepted :: [Workflow] -> [Rating] -> Bool
accepted wfs ratings = go (fromJust $ lookupWF "in" wfs)
  where
    go wf = case runRating wf ratings of
        "A" -> True
        "R" -> False
        xs -> go (fromJust $ lookupWF xs wfs)

defaultRange :: IntSet
defaultRange = Set.fromList [1 .. 4000]

startXMAS :: XMAS
startXMAS = XMAS defaultRange defaultRange defaultRange defaultRange

prodXmas :: XMAS -> Int
prodXmas (XMAS x m a s) = Set.size x * Set.size m * Set.size a * Set.size s

data XMAS = XMAS {x :: IntSet, m :: IntSet, a :: IntSet, s :: IntSet}
    deriving (Show, Eq, Ord)

steps :: [Workflow] -> (String, XMAS) -> [(String, XMAS)]
steps wfs (label, xmas) =
    maybe
        [(label, xmas)]
        (concatMap (steps wfs) . (`step` xmas))
        (lookupWF label wfs)

-- Feels like a scanl moment but I haven't figured it out yet
step :: Workflow -> XMAS -> [(String, XMAS)]
step (WF _ [] els) xmas = [(els, xmas)]
step (WF nm (b : brs) els) xmas = case step' b xmas of
    (label, left, right) -> (label, left) : step (WF nm brs els) right

step' :: Branch -> XMAS -> (String, XMAS, XMAS)
step' (B cat cond sz jmp) (XMAS x m a s) =
    case cat of
        X -> (\(x, x') -> (jmp, XMAS x m a s, XMAS x' m a s)) (filt cond x)
        M -> (\(m, m') -> (jmp, XMAS x m a s, XMAS x m' a s)) (filt cond m)
        A -> (\(a, a') -> (jmp, XMAS x m a s, XMAS x m a' s)) (filt cond a)
        S -> (\(s, s') -> (jmp, XMAS x m a s, XMAS x m a s')) (filt cond s)
  where
    filt :: Condition -> IntSet -> (IntSet, IntSet)
    filt LessThan xs = Set.partition (< sz) xs
    filt GreaterThan xs = Set.partition (> sz) xs

p1 :: String -> Int
p1 s =
    let (wf, rs) = parse s
     in sum (map snd (concat (filter (accepted wf) rs)))

solve :: AOC
solve = AOC 19 p1 p2

p2 :: String -> Int
p2 =
    sum
        . map (prodXmas . snd)
        . filter ((== "A") . fst)
        . (`steps` ("in", startXMAS))
        . fst
        . parse

-- Parsing

categoryP :: Parser Category
categoryP =
    P.char 'x'
        $> X
        <|> P.char 'm'
        $> M
        <|> P.char 'a'
        $> A
        <|> P.char 's'
        $> S

conditionP :: Parser Condition
conditionP =
    P.char '<'
        $> LessThan
        <|> P.char '>'
        $> GreaterThan

branchP :: Parser Branch
branchP = do
    cat <- categoryP
    cond <- conditionP
    sz <- L.decimal
    _ <- P.char ':'
    jmp <- P.some P.alphaNumChar
    pure $ B cat cond sz jmp

workflowP :: Parser Workflow
workflowP = do
    name <- P.some P.alphaNumChar
    _ <- P.char '{'
    bs <- P.try branchP `P.sepEndBy` P.char ','
    e <- P.some P.alphaNumChar
    _ <- P.char '}'
    pure $ WF name bs e

workflowsP :: Parser [Workflow]
workflowsP = workflowP `P.sepEndBy` P.newline

ratingP :: Parser Rating
ratingP = (,) <$> (categoryP <* P.char '=') <*> L.decimal

ratingBlockP :: Parser [Rating]
ratingBlockP = P.char '{' *> (ratingP `P.sepBy` P.char ',') <* P.char '}'

ratingsP :: Parser [[Rating]]
ratingsP = ratingBlockP `P.sepEndBy` P.newline

parse :: String -> ([Workflow], [[Rating]])
parse s =
    ( fromJust $ P.parseMaybe workflowsP wfs
    , fromJust $ P.parseMaybe ratingsP rs
    )
  where
    (wfs, rs) = toTuple (splitOn "\n\n" s)
