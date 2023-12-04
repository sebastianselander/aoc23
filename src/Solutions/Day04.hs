module Solutions.Day04 (solve) where

import Lude

import Data.List.Extra
import Data.Text qualified as Text
import Data.IntMap (IntMap)
import Data.IntMap qualified as M
import Data.Sequence (Seq(..), (><))
import Data.Sequence qualified as Seq

data Card = Card { number :: Int, winning :: [Int], copy :: [Int] }
  deriving Show

parse :: String -> Card
parse s = let [l, r] = splitOn ":" s
              [w, m] = map words $ splitOn "|" r
              num = read $ filter isDigit l
           in Card num (map read w) (map read m)

p :: Text -> [Card]
p = map parse . lines . Text.unpack

p1 :: [Card] -> Int
p1 = sum . map go
  where
    go :: Card -> Int
    go (Card _ wins mine)
      | null (wins `intersect` mine) = 0
      | otherwise = 2 ^ (length (wins `intersect` mine) - 1)

count :: Card -> (Int, Seq Int)
count (Card n w m) = (n, [succ n .. n + length (w `intersect` m)])

run :: [Card] -> IntMap (Seq Int)
run = M.fromList . map count

p2 :: [Card] -> Int
p2 xs = Seq.length $ Seq.fromList (M.keys a) >< go (Seq.fromList $ M.keys a) a
  where
    a = run xs
    go :: Seq Int -> IntMap (Seq Int) -> Seq Int
    go Seq.Empty m = []
    go (x :<| xs) m = (m M.! x) >< go (xs >< m M.! x) m

solve :: AOC
solve = AOC 4 (p1 . p) (p2 . p)
