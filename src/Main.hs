module Main where

import Data.Time.Calendar
import Data.Time.Clock
import Lude (AOC (..), forM_)
import System.Console.ANSI
import System.Directory
import System.Environment
import System.Exit
import System.IO
import Text.Read (readMaybe)
import TextShow
import Prelude
import Prelude qualified as Prel

import Day01 qualified as D1
import Day02 qualified as D2
import Day03 qualified as D3
import Day04 qualified as D4
import Day05 qualified as D5
import Day06 qualified as D6
import Day07 qualified as D7
import Day08 qualified as D8
import Day09 qualified as D9
import Day10 qualified as D10
import Day11 qualified as D11
import Day12 qualified as D12
import Day13 qualified as D13
import Day14 qualified as D14
import Day15 qualified as D15
import Day16 qualified as D16
import Day17 qualified as D17
import Day18 qualified as D18
import Day19 qualified as D19
import Day20 qualified as D20
import Day21 qualified as D21
import Day22 qualified as D22
import Day23 qualified as D23
import Day24 qualified as D24
import Day25 qualified as D25

mains :: [AOC]
mains = [ D1.solve
        , D2.solve
        , D3.solve
        , D4.solve
        , D5.solve
        , D6.solve
        , D7.solve
        , D8.solve
        , D9.solve
        , D10.solve
        , D11.solve
        , D12.solve
        , D13.solve
        , D14.solve
        , D15.solve
        , D16.solve
        , D17.solve
        , D18.solve
        , D19.solve
        , D20.solve
        , D21.solve
        , D22.solve
        , D23.solve
        , D24.solve
        , D25.solve
        ]

inputFilePrefix :: String
inputFilePrefix = "day"

inputDir :: String
inputDir = "inputs/"

makeISO :: Year -> MonthOfYear -> DayOfMonth -> String
makeISO year month day =
    show year
        <> "-"
        <> show month
        <> "-"
        <> show day

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

instance TextShow Part where
    showb Part1 = fromText "1"
    showb Part2 = fromText "2"

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
        ["all"] -> forM_ mains (\(AOC day p1 p2) -> do
                        putStrLn $ "==== Day" <> show day <> " ===="
                        input <- parseFile $ inputDir <> inputFilePrefix <> show day
                        pre <- getCurrentTime
                        putStrLn $ "Part1: " <> show (p1 input)
                        putStrLn $ "Part2: " <> show (p2 input)
                        putStr "    "
                        timePost pre
                        putStrLn "")
        [inputDay'] -> do
            let inputDay = inputDay'
            dayNumber <- parseDay inputDay
            input <- parseFile $ inputDir <> inputFilePrefix <> inputDay
            res1 <- execute dayNumber Part1 mains input
            res2 <- execute dayNumber Part2 mains input
            putStrLn $ "Part1: " <> res1
            putStrLn $ "Part2: " <> res2
            timePost pre
        [inputDay', inputPart'] -> do
            let inputDay = inputDay'
            let inputPart = inputPart'
            dayNumber <- parseDay inputDay
            partNumber <- parsePart inputPart
            input <- parseFile $ inputDir <> inputFilePrefix <> inputDay
            res <- execute dayNumber partNumber mains input
            putStrLn $
                "Part"
                    <> show partNumber
                    <> ": "
                    <> res
            timePost pre
        [inputDay', inputPart', inputFile] -> do
            let inputDay = inputDay'
            let inputPart = inputPart'
            dayNumber <- parseDay inputDay
            partNumber <- parsePart inputPart
            input <- parseFile inputFile
            res <- execute dayNumber partNumber mains input
            putStrLn $
                "Part"
                    <> show partNumber
                    <> ": "
                    <> res
            timePost pre
        [] -> do
            input <- parseFile $ inputDir <> inputFilePrefix <> show day
            res1 <- execute day Part1 mains input
            res2 <- execute day Part2 mains input
            putStrLn $ "Part1: " <> res1
            putStrLn $ "Part2: " <> res2
            timePost pre
        xs ->
            errExit $
                concat
                    [ "Incorrect amount of arguments: "
                    , quote $ show (Prel.length xs)
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
quote str = "'" <> str <> "'"
