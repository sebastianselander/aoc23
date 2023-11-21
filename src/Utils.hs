{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Utils
  ( Rect (..),
    overlap,
    module Data.List.Extra,
    module Data.Function.Memoize,
    module Data.Bifunctor,
    module Data.Foldable,
    module Data.Traversable,
    module Data.Void,
    module Data.Maybe,
    module Data.Bool,
    module Control.Applicative,
    module Control.Monad,
    module Data.Functor,
    module Data.Char,
    opPairs,
    both,
    manhattan,
    onAllOther,
    todo,
  )
where

import Control.Applicative
import Control.Monad
import Data.Bifunctor
import Data.Bool
import Data.Char
import Data.Foldable
import Data.Traversable
import Data.Function.Memoize
import Data.Functor
import Data.List.Extra
import Data.Maybe
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Void
import Test.QuickCheck (Arbitrary (arbitrary), Gen)

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
  { xCoord :: Int,
    yCoord :: Int,
    extendX :: Int,
    extendY :: Int
  }
  deriving (Show)

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
    go _ ys Seq.Empty = Seq.Empty
    go n ys (x :<| xs) = go' n x ys :<| go (n + 1) ys xs

    go' :: Int -> a -> Seq a -> Seq b
    go' index e Seq.Empty = Seq.Empty
    go' 0 e (x :<| xs) = go' (-1) e xs
    go' n e (x :<| xs) = f e x :<| go' (n - 1) e xs

todo :: a
todo = error "TODO"

-- Tests

instance Arbitrary Rect where
  arbitrary :: Gen Rect
  arbitrary =
    Rect
      <$> fmap abs arbitrary
      <*> fmap abs arbitrary
      <*> fmap abs arbitrary
      <*> fmap abs arbitrary

prop_overlapIdentity :: Rect -> Bool
prop_overlapIdentity r1 = overlap r1 r1

prop_overlapComm :: Rect -> Rect -> Bool
prop_overlapComm r1 r2 = overlap r1 r2 == overlap r2 r1
