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
    Rect (..),
    overlap,
    AOC (..),
    opPairs,
    both,
    manhattan,
    onAllOther,
    applyN,
    todo,
    Parser,
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
import Data.Functor
import Data.Int
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.String
import Data.Traversable
import Data.Tuple
import Data.Void (Void)
import GHC.IO.Unsafe (unsafePerformIO)
import Text.Megaparsec (Parsec)
import Unsafe.Coerce
import Data.Text
import TextShow

type Parser = Parsec Void Text

data AOC = forall a b.
      (TextShow a, TextShow b) =>
    AOC
    { day :: Int
    , part1 :: Text -> a
    , part2 :: Text -> b
    }

{-
..........
..........
...####...
...####...
...####...
..........
..........
Represented as:
    Rect {
        xCorrd = 3,
        yCoord = 2,
        extendX = 3,
        extendY = 2
    }
-}

data Rect = Rect
    { xCoord :: Int
    , yCoord :: Int
    , extendX :: Int
    , extendY :: Int
    }
    deriving (Show)

applyN :: Int -> (a -> a) -> a -> a
applyN n f x = go x n f x
  where
    go acc 0 _ _ = acc
    go !acc n f x = go (f acc) (n - 1) f x

opPairs :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
opPairs f (a, aa) (b, bb) = (f a b, f aa bb)

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

onAllOther :: forall a b. (a -> a -> b) -> [a] -> [[b]]
onAllOther f xs =
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
