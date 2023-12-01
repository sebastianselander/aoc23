{-# LANGUAGE QuasiQuotes #-}

module Solutions.Day1 (solve) where

import Data.Text (Text)
import Data.Text qualified as T
import Lude
import Text.RE.TDFA.Text

p1 :: String -> Int
p1 = sum
   . map (uncurry (+)
          . ((10*) . head &&& last)
          . map digitToInt
          . filter isDigit)
   . lines

p2 :: String -> Int
p2 = p1 . T.unpack . replace . T.pack

replace :: Text -> Text
replace s =
    s
        *=~/ [ed|one///o1e|]
        *=~/ [ed|two///t2o|]
        *=~/ [ed|three///t3e|]
        *=~/ [ed|four///f4r|]
        *=~/ [ed|five///f5e|]
        *=~/ [ed|six///s6x|]
        *=~/ [ed|seven///s7n|]
        *=~/ [ed|eight///e8t|]
        *=~/ [ed|nine///n9e|]

solve :: AOC
solve = AOC 1 p1 p2
