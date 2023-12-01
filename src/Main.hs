{-# LANGUAGE ImpredicativeTypes #-}
module Main where

import Text.Read (readMaybe)
import Data.Time.Calendar
import Data.Time.Clock
import Lude (AOC (..))
import System.Console.ANSI
import System.Directory
import System.Environment
import System.Exit
import System.IO

import Solutions.Day1 qualified as Day1

mains :: [AOC]
mains = [Day1.solve]

inputFilePrefix :: String
inputFilePrefix = "day"

inputDir :: FilePath
inputDir = "inputs/"

makeISO :: Year -> MonthOfYear -> DayOfMonth -> String
makeISO year month day =
     show year
        <> "-"
        <>  show month
        <> "-"
        <>  show day

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
        [ SetConsoleIntensity BoldIntensity
        , SetColor Foreground Dull Green
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
    pre <- getCurrentTime
    case args of
        [inputDay] -> do
            dayNumber <- parseDay inputDay
            input <- parseFile (inputDir <> inputFilePrefix <> inputDay)
            res1 <- execute dayNumber Part1 mains input
            res2 <- execute dayNumber Part2 mains input
            putStrLn $ "Part1: " <> res1
            putStrLn $ "Part2: " <> res2
            timePost pre
        [inputDay, inputPart] -> do
            dayNumber <- parseDay inputDay
            partNumber <- parsePart inputPart
            input <- parseFile (inputDir <> inputFilePrefix <> inputDay)
            res <- execute dayNumber partNumber mains input
            putStrLn $
                "Part"
                    <>  show partNumber
                    <> ": "
                    <> res
            timePost pre
        [inputDay, inputPart, inputFile] -> do
            dayNumber <- parseDay inputDay
            partNumber <- parsePart inputPart
            input <- parseFile inputFile
            res <- execute dayNumber partNumber mains input 
            putStrLn $
                "Part"
                    <>  show partNumber
                    <> ": "
                    <> res
            timePost pre
        [] -> do
            input <- parseFile (inputDir <> inputFilePrefix <> show day)
            res1 <- execute day Part1 mains input
            res2 <- execute day Part2 mains input
            putStrLn $ "Part1: " <> res1
            putStrLn $ "Part2: " <> res2
            timePost pre
        xs ->
            errExit $
                concat
                    [ "Incorrect amount of arguments: "
                    , quote $ show (length xs)
                    , "\n\n"
                    , "The program is run by one of the following commands:\n"
                    , quote "cabal run aoc23 -- <DAY> <PART> <INPUTFILE>"
                    , "\nor\n"
                    , quote "cabal run aoc23 -- <DAY> <PART>"
                    , "\nor\n"
                    , quote "cabal run aoc23 -- <DAY>"
                    ]

timePost :: UTCTime -> IO ()
timePost pre = do
    post <- getCurrentTime
    let diff = diffUTCTime post pre
    putStrLn $ "\nTook " <> show diff

execute :: Int -> Part -> [AOC] -> String -> IO String
execute n part xs input = do
    (AOC _ p1 p2) <- find n xs
    case part of
        Part1 -> pure . show $ p1 input
        Part2 -> pure . show $ p2 input
  where
    find :: Int -> [AOC] -> IO AOC
    find _ [] =
        errExit $ "Program for day " <> quote (show n) <> " does not exist"
    find m (y : ys)
        | m == day y = pure y
        | otherwise = find m ys

parseFile :: String -> IO String
parseFile inputFile =
    doesFileExist inputFile >>= \case
        False ->
            hPutStrLn
                stderr
                ("The file " <> quote inputFile <> " does not exist")
                >> exitFailure
        True -> readFile inputFile

parseDay :: String -> IO Int
parseDay day
    | Just n <- readMaybe @Int day
    , n >= 1 && n <= 25 =
        pure n
    | otherwise =
        errExit $
            unwords
                [ "Could not parse day"
                , quote day
                , "as an integer in the range [1 .. 25]"
                ]

parsePart :: String -> IO Part
parsePart part
    | Just 1 <- readMaybe @Int part = pure Part1
    | Just 2 <- readMaybe @Int part = pure Part2
    | otherwise = do
        errExit $
            unwords
                [ "Could not parse part"
                , quote part
                , "as an integer in the range [1 .. 2]"
                ]

errExit :: String -> IO a
errExit str = hPutStrLn stderr str >> exitFailure

quote :: String -> String
quote str = concat ["'", str, "'"]
