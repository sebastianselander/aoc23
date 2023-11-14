{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import Data.Time.Calendar
import Data.Time.Clock
import System.Directory
import System.Environment
import System.Exit
import System.IO
import Text.Read

mains :: [(Int, String -> IO ())]
mains = []

inputFilePrefix :: FilePath
inputFilePrefix = "day"

inputDir :: FilePath
inputDir = "inputs/"

makeISO :: Year -> MonthOfYear -> DayOfMonth -> String
makeISO year month day = concat [show year, "-", show month, "-", show day]

main :: IO ()
main = do
    args <- getArgs
    (year, month, day) <- toGregorian . utctDay <$> getCurrentTime
    putStrLn $ "Today is: " ++ makeISO year month day
    case args of
        [] -> do
            f <- execute day mains
            file <- parseFile (inputDir ++ inputFilePrefix ++ show day)
            f file
        [inputDay] -> do
            dayNumber <- case parseDay inputDay of
                Nothing ->
                    errExit
                        $ unwords
                            [ "Could not parse day"
                            , quote inputDay
                            , "as an integer in the range [1 .. 25]"
                            ]
                Just n -> pure n
            f <- execute dayNumber mains
            input <- parseFile (inputDir ++ inputFilePrefix ++ inputDay)
            f input
        xs ->
            errExit
                $ concat
                    [ "Incorrect amount of arguments "
                    , quote $ show (length xs)
                    , "\n\n"
                    , "The program is run by the following command: 'cabal run aoc23 -- <FILE> <DAY>'"
                    , "\n"
                    , "<DAY> is optional, if left blank it runs the current day"
                    ]

execute :: Int -> [(Int, String -> IO ())] -> IO (String -> IO ())
execute n fs = case lookup n fs of
    Just f -> pure f
    Nothing -> errExit $ "Program for day " ++ quote (show n) ++ " does not exist"

parseFile :: String -> IO String
parseFile inputFile =
    doesFileExist inputFile >>= \case
        False ->
            hPutStrLn stderr ("The file " ++ quote inputFile ++ " does not exist")
                >> exitFailure
        True -> readFile inputFile

parseDay :: String -> Maybe Int
parseDay day
    | Just n <- readMaybe @Int day
    , n >= 1 && n <= 25 =
        Just n
    | otherwise = Nothing

errExit :: String -> IO a
errExit str = hPutStrLn stderr str >> exitFailure

quote :: String -> String
quote str = concat ["'", str, "'"]
