{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Lude (
    module Control.Arrow,
    module Control.Monad.Zip,
    module Control.Monad,
    module Control.Applicative,
    module Data.Bifoldable,
    module Data.Bits,
    module Data.Bool,
    module Data.Char,
    module Data.Either,
    module Data.Function,
    module Data.Functor,
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
    module Data.Function.Memoize,
    Rect (..),
    overlap,
    AOC (..),
    opPairs,
    both,
    manhattan,
    onOthers,
    apN,
    todo,
    listToTuple,
    setAt,
    pertubations,
    pertubationsBy,
    findIndicesElem,
    slidingWindows,
    freqs,
    fixed,
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
import Data.Either
import Data.Foldable
import Data.Function
import Data.Function.Memoize
import Data.Functor
import Data.Int
import Data.List
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.String
import Data.Text (Text)
import Data.Traversable
import Data.Tuple
import Data.Void (Void)
import GHC.IO.Unsafe (unsafePerformIO)
import Text.Megaparsec (Parsec)
import TextShow (TextShow)
import Unsafe.Coerce

type Parser = Parsec Void Text


data AOC = forall a b.
      (TextShow a, TextShow b) =>
    AOC
    { day :: Int
    , part1 :: Text -> a
    , part2 :: Text -> b
    }

instance Show AOC where
    show (AOC n _ _) =
        "AOC contains functions, but this one represents day " ++ show n

slidingWindows :: forall a. Int -> [a] -> [[a]]
slidingWindows n l = take n <$> tails l

listToTuple :: [a] -> (a, a)
listToTuple [a, b] = (a, b)
listToTuple _ = error "ERROR: more than two elements"

freqs :: (Foldable f, Ord a) => f a -> Map a Int
freqs = M.fromListWith (+) . map (,1) . toList

-- | Bottoms if the index is out of bounds
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
    go n ys (x :<| xs) = go' n x ys :<| go (n + 1) ys xs

    go' :: Int -> a -> Seq a -> Seq b
    go' _ _ Seq.Empty = Seq.Empty
    go' 0 e (_ :<| xs) = go' (-1) e xs
    go' n e (x :<| xs) = f e x :<| go' (n - 1) e xs

todo :: a
todo = error "TODO"
