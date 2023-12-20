{-# LANGUAGE OverloadedRecordDot #-}

module Lude (
    module Control.Arrow,
    module Control.Monad.Zip,
    module Control.Monad,
    module Control.Applicative,
    module Data.Bifoldable,
    module Data.Bits,
    module Data.Bool,
    module Data.Char,
    module Data.Composition,
    module Data.Either,
    module Data.Function,
    module Data.Functor,
    module Data.Functor.Classes,
    module Data.Ix,
    module Data.Int,
    module Data.List,
    module Data.Maybe,
    module Data.Monoid,
    module Data.Ord,
    module Data.String,
    module Data.Traversable,
    module Data.Tuple,
    module GHC.IO.Unsafe,
    module Unsafe.Coerce,
    Rect (..),
    AOC (..),
    overlap,
    opPairs,
    both,
    manhattan,
    onOthers,
    apN,
    todo,
    toTuple,
    setAt,
    elemOn,
    pertubations,
    pertubationsBy,
    findIndicesElem,
    slidingWindows,
    freqs,
    fixed,
    byOrder,
    list,
    vec,
    seq,
    set,
    pos,
    safeTail,
    countElem,
    test,
    (&.&),
    Parser,
    Text,
)
where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Zip
import Data.Bifoldable
import Data.Bifunctor
import Data.Bits
import Data.Bool
import Data.Char
import Data.Composition
import Data.Either
import Data.Foldable
import Data.Function
import Data.Functor
import Data.Functor.Classes
import Data.Int
import Data.Ix
import Data.List hiding (map)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String
import Data.Text (Text)
import Data.Traversable
import Data.Tuple
import Data.Vector (Vector)
import Data.Vector qualified as Vec
import Data.Void (Void)
import GHC.IO.Unsafe (unsafePerformIO)
import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as P
import Unsafe.Coerce
import Prelude hiding (map, seq)

test :: String
{-# NOINLINE test #-}
test = unsafePerformIO $ readFile "test/test"

type Parser = Parsec Void String

data AOC = forall a b.
      (Show a, Show b) =>
    AOC
    { day :: Int
    , part1 :: String -> a
    , part2 :: String -> b
    }

instance Show AOC where
    show (AOC n _ _) =
        "AOC contains functions, but this one represents day " ++ show n

{- | Bottom if the list does not contain both elements
  Quite slow probaby
-}
byOrder :: (Ord a) => [a] -> a -> a -> Ordering
byOrder xs y z = compare (fromJust $ elemIndex y xs) (fromJust $ elemIndex z xs)

slidingWindows :: forall a. Int -> [a] -> [[a]]
slidingWindows n l = take n <$> tails l

toTuple :: forall a f. (Foldable f) => f a -> (a, a)
toTuple xs = case toList xs of
    [l, r] -> (l, r)
    _ -> error "ERROR: more than two elements"

-- | Returns a map of frequencies of elements
freqs :: (Foldable f, Ord a) => f a -> Map a Int
freqs = M.fromListWith (+) . foldr ((:) . (,1)) mempty

-- | Error if the index is out of bounds
setAt :: Int -> a -> [a] -> [a]
setAt n x = (\(l, r) -> l ++ x : tail r) . splitAt n

pertubationsBy :: (a -> Bool) -> (a -> a) -> [a] -> [[a]]
pertubationsBy p f l = [setAt n (f x) l | (x, n) <- findIndicesElem p l]

findIndicesElem :: (Foldable t) => (a -> Bool) -> t a -> [(a, Int)]
findIndicesElem p = reverse . fst . foldl' go ([], 0)
  where
    go (l, n) x
        | p x = ((x, n) : l, n + 1)
        | otherwise = (l, n + 1)

-- | Unconditional pertubation
pertubations :: (a -> a) -> [a] -> [[a]]
pertubations = pertubationsBy (const True)

apN :: Int -> (a -> a) -> a -> a
apN 0 _ !x = x
apN !n f !x = apN (n - 1) f (f x)

opPairs :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
opPairs f (a, aa) (b, bb) = (f a b, f aa bb)

fixed :: (Eq a) => (a -> a) -> a -> a
fixed f !x = if x == y then x else fixed f y
  where
    y = f x

elemOn :: (Eq b, Foldable f) => (a -> b) -> b -> f a -> Bool
elemOn f e = foldr ((||) . (== e) . f) False

data Rect = Rect
    { xCoord :: Int
    , yCoord :: Int
    , extendX :: Int
    , extendY :: Int
    }
    deriving (Show)

overlap :: Rect -> Rect -> Bool
overlap rect1 rect2 =
    not $
        rect1.xCoord + rect1.extendX < rect2.xCoord
            && rect1.xCoord > rect2.xCoord + rect2.extendX
            || rect1.yCoord + rect1.extendY < rect2.yCoord
                && rect1.yCoord > rect2.yCoord + rect2.extendY

both :: (Bifunctor f) => (a -> b) -> f a a -> f b b
both f = bimap f f

manhattan :: (Int, Int) -> (Int, Int) -> Int
manhattan (x, y) (xx, yy) = abs (xx - x) + abs (yy - y)

onOthers :: forall a b. (a -> a -> b) -> [a] -> [[b]]
onOthers f xs =
    let seq = Seq.fromList xs
     in toList (toList <$> go 0 seq seq)
  where
    go :: Int -> Seq a -> Seq a -> Seq (Seq b)
    go _ _ Seq.Empty = Seq.Empty
    go n ys (x :<| xs) = go2 n x ys :<| go (n + 1) ys xs
      where
        go2 :: Int -> a -> Seq a -> Seq b
        go2 _ _ Seq.Empty = Seq.Empty
        go2 0 e (_ :<| xs) = go2 (-1) e xs
        go2 n e (x :<| xs) = f e x :<| go2 (n - 1) e xs

-- Might be buggy
pos :: Parser (Int, Int)
pos =
    (pred . pred . P.unPos . P.sourceColumn &&& pred . P.unPos . P.sourceLine)
        <$> P.getSourcePos

safeTail :: [a] -> [a]
safeTail [] = []
safeTail xs = tail xs

todo :: a
todo = error "TODO"

countElem :: (Foldable f, Eq a) => a -> f a -> Int
countElem e = foldr (\x acc -> if x == e then acc + 1 else acc) 0

-- | Short circuits on the second argument
(&.&) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(&.&) f g a = g a && f a

-- Vector

seq :: (Foldable f) => f a -> Seq a
seq = foldr (Seq.<|) Seq.empty

list :: (Foldable f) => f a -> [a]
list = toList

vec :: (Foldable f) => f a -> Vector a
vec = foldr Vec.cons Vec.empty

set :: (Foldable f, Ord a) => f a -> Set a
set = foldr Set.insert Set.empty
