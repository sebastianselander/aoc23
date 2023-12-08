{-# LANGUAGE OverloadedLists #-}
module Day04 (solve) where

import Lude

import Data.IntMap (IntMap)
import Data.IntMap qualified as M
import Data.List.Extra
import Data.Sequence (Seq (..), (><))
import Data.Sequence qualified as Seq
import Data.Text qualified as Text

data Card = Card {number :: Int, winning :: [Int], copy :: [Int]}
    deriving (Show)

parse :: Text -> [Card]
parse = map p . lines . Text.unpack
  where
    p :: String -> Card
    p s =
        let [l, r] = splitOn ":" s
            [w, m] = map words $ splitOn "|" r
            num = read $ filter isDigit l
         in Card num (map read w) (map read m)

p1 :: Text -> Int
p1 = sum . map go . parse
  where
    go :: Card -> Int
    go (Card _ wins mine)
        | null (wins `intersect` mine) = 0
        | otherwise = 2 ^ (length (wins `intersect` mine) - 1)

count :: Card -> (Int, Seq Int)
count (Card n w m) = (n, [succ n .. n + length (w `intersect` m)])

run :: [Card] -> IntMap (Seq Int)
run = M.fromList . map count

p2 :: Text -> Int
p2 xs = Seq.length $ Seq.fromList (M.keys a) >< go (Seq.fromList $ M.keys a) a
  where
    a = run $ parse xs
    go :: Seq Int -> IntMap (Seq Int) -> Seq Int
    go Seq.Empty _ = []
    go (x :<| ys) m = (m M.! x) >< go (ys >< m M.! x) m

solve :: AOC
solve = AOC 4 p1 p2
