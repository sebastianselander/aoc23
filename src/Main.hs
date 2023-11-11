{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import           Data.Time.Calendar
import           Data.Time.Clock
import           System.Directory
import           System.Environment
import           System.Exit
import           System.IO
import           Text.Read

mains :: [String -> IO ()]
mains =
    let
        impls = []
    in
        impls ++ replicate
                    (25 - length impls)
                    (putStrLn . const "Not yet implemented")

main :: IO ()
main = do
    args <- getArgs
    (_, _, currentDay) <-  toGregorian . utctDay <$> getCurrentTime
    case args of
        [input] -> parseFile input >>= execute currentDay mains
        [input, day] -> do
            input <- parseFile input
            dayNumber <- case parseDay day of
                Nothing -> errExit $ "Could not parse day " ++ quote day ++ " as an integer in the range [1 .. 25]"
                Just n  -> pure n
            execute dayNumber mains input
        xs -> errExit $ concat
            [ "Incorrect amount of arguments "
            , quote $ show (length xs)
            , "\n\n"
            , "The program is run by the following command: 'cabal run aoc23 -- <FILE> <DAY>'"
            , "\n"
            , "<DAY> is optional, if left blank it runs the current day"]

execute :: Int -> [String -> IO ()] -> String -> IO ()
execute n fs input
  | length fs >= n = (fs !! (n - 1)) input
  | otherwise         = errExit $ "Program for day " ++ quote (show n) ++ " does not exist"

parseFile :: String -> IO String
parseFile inputFile
    = doesFileExist inputFile >>= \case
        False -> hPutStrLn stderr ("The file " ++ quote inputFile ++ " does not exist")
                 >> exitFailure
        True -> readFile inputFile

parseDay :: String -> Maybe Int
parseDay day
  | Just n <- readMaybe @Int day
  , n >= 1 && n <= 25 = Just n
  | otherwise = Nothing

errExit :: String -> IO a
errExit str = hPutStrLn stderr str >> exitFailure

quote :: String -> String
quote str = concat ["'", str, "'"]
