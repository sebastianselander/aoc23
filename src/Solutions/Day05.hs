{-# LANGUAGE OverloadedRecordDot #-}
module Solutions.Day05 (solve) where

import Data.Text (pack, unpack)
import Data.Text qualified as Text
import Data.List.Extra
import Lude

import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as L
import Debug.Trace (traceShow, traceShowId)

testData :: Text
testData = pack $ unsafePerformIO $ readFile "test/day5_1"

inp = pack $ unsafePerformIO $ readFile "inputs/day5"

data Almanac = Alm
    { seeds :: [Int]
    , seed_to_soil :: [ConversionMap]
    , soil_to_fertilizer :: [ConversionMap]
    , fertilizer_to_water :: [ConversionMap]
    , water_to_light :: [ConversionMap]
    , light_to_temperature :: [ConversionMap]
    , temperature_to_humidity :: [ConversionMap]
    , humidity_to_location :: [ConversionMap]
    } deriving (Show)

data ConversionMap = CM
    { destRange :: Int
    , srcRange :: Int
    , rangeLen :: Int
    }
    deriving (Show)

pMap :: Parser ConversionMap
pMap = CM <$> L.decimal <* P.space <*> L.decimal <* P.space <*> L.decimal

pSeeds :: Parser [Int]
pSeeds = "seeds: " *> P.some (L.decimal <* P.optional P.space)

pTo :: Text -> Parser [ConversionMap]
pTo str = do
    _ <- P.string (str <> " map:") <* P.newline
    pMap `P.sepBy` P.newline

-- parse :: Text -> Almanac
parse t = Alm seeds soil fert water light temp humid loc
  where
    c     = map pack $ splitOn "\n\n" (dropEnd 1 $ unpack t)
    seeds = fromJust $ P.parseMaybe pSeeds (head c)
    soil  = fromJust $ P.parseMaybe (pTo "seed-to-soil") (c !! 1)
    fert  = fromJust $ P.parseMaybe (pTo "soil-to-fertilizer") (c !! 2)
    water = fromJust $ P.parseMaybe (pTo "fertilizer-to-water") (c !! 3)
    light = fromJust $ P.parseMaybe (pTo "water-to-light") (c !! 4)
    temp  = fromJust $ P.parseMaybe (pTo "light-to-temperature") (c !! 5)
    humid = fromJust $ P.parseMaybe (pTo "temperature-to-humidity") (c !! 6)
    loc   = fromJust $ P.parseMaybe (pTo "humidity-to-location") (c !! 7)

-- Bug below

simulate :: Almanac -> Int
simulate m = minimum $ map (run m) m.seeds

run :: Almanac -> Int -> Int
run alm n = snd $ foldl' runSeed (False, n) ([
    alm.seed_to_soil ,
    alm.soil_to_fertilizer ,
    alm.fertilizer_to_water ,
    alm.water_to_light ,
    alm.light_to_temperature ,
    alm.temperature_to_humidity ,
    alm.humidity_to_location ] :: [[ConversionMap]])

-- WHy can I not do this
-- runSeed :: (Bool, Int) -> [ConversionMap] -> (Bool, Int)
-- runSeed n = first (const False) . foldl' runSeedOnce n

runSeed :: (Bool, Int) -> [ConversionMap] -> (Bool, Int)
runSeed p xs
  | null a = p
  | otherwise = first (const False) $ head a
    where
      a = filter fst $ map (runSeedOnce p) xs

runSeedOnce :: (Bool, Int) -> ConversionMap -> (Bool, Int)
runSeedOnce (b, n) convMap
  | inRange n && not b = (True, n + (convMap.destRange - convMap.srcRange))
  | otherwise          = (False, n)
  where
    inRange :: Int -> Bool
    inRange m = m >= convMap.srcRange
            && m <= convMap.srcRange + (convMap.rangeLen - 1)

p1 :: Text -> Int
p1 = simulate . parse

newSeeds :: Almanac -> Almanac
newSeeds alm = alm {seeds = concat $ gen alm.seeds}
  where
    gen :: [Int] -> [[Int]]
    gen [] = []
    gen (x:y:xs) = take y [x ..] : gen xs
    gen (x:_) = error "uneven number of seeds"

p2 :: Text -> Int
p2 = simulate . newSeeds . parse

solve :: AOC
solve = AOC 5 p1 p2
