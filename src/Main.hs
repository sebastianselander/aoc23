module Main where

import           Control.Monad
import           Data.Time.Calendar
import           Data.Time.Clock
import           System.Environment

mains :: [String -> IO ()]
mains = undefined

main :: IO ()
main = do
    -- cabal run aoc23 -- <FILE>
    args <- getArgs
    (_, _, currentDay) <-  toGregorian . utctDay <$> getCurrentTime
    case args of
        [input]      -> mains !! currentDay $ input
        [input, day] -> (mains !! read day) input
        xs            -> error $ "Incorrect amount of arguments: " ++ show (length xs)
