{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Solutions.Day1 (solve) where

import Data.Char
import Data.Text (Text)
import Data.Text qualified as Text
import Lude

p1 :: Text -> Text
p1 = Text.pack . show . sum . map (f . Text.filter isDigit) . Text.lines
  where
    f :: Text -> Int
    f xs =
        let (first, last) = (Text.head xs, Text.last xs)
         in read $ first : [last]

p2 :: Text -> Text
p2 = Text.pack . show . sum . map (g . ugly) . Text.lines
  where
    g :: [Int] -> Int
    g xs = head xs * 10 + last xs

ugly :: Text -> [Int]
ugly s
  -- Really tail instead of drop (length of prefix)... :(
  | Text.isPrefixOf "one" s   = 1 : ugly (Text.tail s)
  | Text.isPrefixOf "two" s   = 2 : ugly (Text.tail s)
  | Text.isPrefixOf "three" s = 3 : ugly (Text.tail s)
  | Text.isPrefixOf "four" s  = 4 : ugly (Text.tail s)
  | Text.isPrefixOf "five" s  = 5 : ugly (Text.tail s)
  | Text.isPrefixOf "six" s   = 6 : ugly (Text.tail s)
  | Text.isPrefixOf "seven" s = 7 : ugly (Text.tail s)
  | Text.isPrefixOf "eight" s = 8 : ugly (Text.tail s)
  | Text.isPrefixOf "nine" s  = 9 : ugly (Text.tail s)
  | Text.isPrefixOf "1"   s   = 1 : ugly (Text.tail s)
  | Text.isPrefixOf "2"   s   = 2 : ugly (Text.tail s)
  | Text.isPrefixOf "3"   s   = 3 : ugly (Text.tail s)
  | Text.isPrefixOf "4"   s   = 4 : ugly (Text.tail s)
  | Text.isPrefixOf "5"   s   = 5 : ugly (Text.tail s)
  | Text.isPrefixOf "6"   s   = 6 : ugly (Text.tail s)
  | Text.isPrefixOf "7"   s   = 7 : ugly (Text.tail s)
  | Text.isPrefixOf "8"   s   = 8 : ugly (Text.tail s)
  | Text.isPrefixOf "9"   s   = 9 : ugly (Text.tail s)
  | Text.null s = []
  | otherwise = ugly (Text.tail s)

solve :: AOC
solve = AOC 1 p1 p2
