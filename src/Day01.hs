{-# LANGUAGE QuasiQuotes #-}

module Day01 (solve) where

import Lude
import Text.RE.TDFA.String

p1 :: String -> Int
p1 =
    sum
        . map
            ( uncurry (+)
                . ((10 *) . head &&& last)
                . map digitToInt
                . filter isDigit
            )
        . lines

p2 :: String -> Int
p2 = p1 . replace

replace :: String -> String
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
