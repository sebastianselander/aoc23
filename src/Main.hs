{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}

module Main where

import Data.Time.Calendar
import Data.Time.Clock
import System.Console.ANSI
import System.Directory
import System.Environment
import System.Exit
import System.IO
import Text.Read

type AOC = (Int, String -> String, String -> String)

mains :: [AOC]
mains = []

inputFilePrefix :: FilePath
inputFilePrefix = "day"

inputDir :: FilePath
inputDir = "inputs/"

makeISO :: Year -> MonthOfYear -> DayOfMonth -> String
makeISO year month day = concat [show year, "-", show month, "-", show day]

clean :: IO ()
clean = setCursorPosition 0 0 >> clearScreen

prettyDate :: Year -> MonthOfYear -> DayOfMonth -> IO ()
prettyDate y m d = do
  let today = "Today's date: "
  let date = makeISO y m d
  putStr today
  setSGR [SetItalicized True]
  putStrLn date
  setSGR [Reset]
  setSGR
    [ SetConsoleIntensity BoldIntensity,
      SetColor Foreground Dull Green
    ]
  putStrLn $ titleBar (length today + length date)
  setSGR [Reset]
  hFlush stdout

titleBar :: Int -> String
titleBar n = replicate n '='

data Part = Part1 | Part2

instance Show Part where
  show Part1 = "1"
  show Part2 = "2"

main :: IO ()
main = do
  clean
  args <- getArgs
  (year, month, day) <- toGregorian . utctDay <$> getCurrentTime
  prettyDate year month day
  case args of
    [inputDay] -> do
      dayNumber <- parseDay inputDay
      f1 <- execute dayNumber Part1 mains
      f2 <- execute dayNumber Part2 mains
      input <- parseFile (inputDir ++ inputFilePrefix ++ inputDay)
      putStrLn $ "Part1: " ++ f1 input
      putStrLn $ "Part2: " ++ f2 input
    [inputDay, inputPart] -> do
      dayNumber <- parseDay inputDay
      partNumber <- parsePart inputPart
      input <- parseFile (inputDir ++ inputFilePrefix ++ inputDay)
      f <- execute dayNumber partNumber mains
      putStrLn $ "Part" ++ show partNumber ++ ": " ++ f input
    [inputDay, inputPart, inputFile] -> do
      dayNumber <- parseDay inputDay
      partNumber <- parsePart inputPart
      input <- parseFile inputFile
      f <- execute dayNumber partNumber mains
      putStrLn $ "Part" ++ show partNumber ++ ": " ++ f input
    xs ->
      errExit $
        concat
          [ "Incorrect amount of arguments: ",
            quote $ show (length xs),
            "\n\n",
            "The program is run by one of the following commands:\n",
            quote "cabal run aoc23 -- <DAY> <PART> <INPUTFILE>",
            "\nor\n",
            quote "cabal run aoc23 -- <DAY> <PART>",
            "\nor\n",
            quote "cabal run aoc23 -- <DAY>"
          ]

execute :: Int -> Part -> [AOC] -> IO (String -> String)
execute n part fs = do
  let f = case part of
        Part1 -> (\(a, b, _) -> (a, b))
        Part2 -> (\(a, _, c) -> (a, c))
  case lookup n (map f fs) of
    Just f -> pure f
    Nothing -> errExit $ "Program for day " ++ quote (show n) ++ " does not exist"

parseFile :: String -> IO String
parseFile inputFile =
  doesFileExist inputFile >>= \case
    False ->
      hPutStrLn stderr ("The file " ++ quote inputFile ++ " does not exist")
        >> exitFailure
    True -> readFile inputFile

parseDay :: String -> IO Int
parseDay day
  | Just n <- readMaybe @Int day,
    n >= 1 && n <= 25 =
      pure n
  | otherwise =
      errExit $
        unwords
          [ "Could not parse day",
            quote day,
            "as an integer in the range [1 .. 25]"
          ]

parsePart :: String -> IO Part
parsePart part
  | Just 1 <- readMaybe @Int part = pure Part1
  | Just 2 <- readMaybe @Int part = pure Part2
  | otherwise = do
      errExit $
        unwords
          [ "Could not parse part",
            quote part,
            "as an integer in the range [1 .. 2]"
          ]

errExit :: String -> IO a
errExit str = hPutStrLn stderr str >> exitFailure

quote :: String -> String
quote str = concat ["'", str, "'"]
