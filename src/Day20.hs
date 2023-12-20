module Day20 (solve) where

import Control.Monad.State (State, gets, modify, runState)
import Data.List.Extra (splitOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Lude

data Status = On | Off
    deriving (Show, Eq, Ord)

data Pulse = Low | High
    deriving (Eq, Ord)

data Module
    = Broadcaster [String]
    | FlipFlop [String] Status
    | Conjunction [String] (Map String Pulse)
    deriving (Eq, Ord)

previous :: Module -> [String]
previous (Conjunction _ m) = Map.keys m
previous _ = []

tos :: Module -> [String]
tos (Broadcaster t) = t
tos (FlipFlop t _) = t
tos (Conjunction t _) = t

data S = S
    { low :: Int
    , high :: Int
    , modMap :: Map String Module
    , presses :: Int
    , lcmMap :: Map String Int
    , rxCon :: (String, [String])
    }
    deriving (Eq, Ord)

increment :: Pulse -> State S ()
increment Low = modify (\c -> c {low = c.low + 1})
increment High = modify (\c -> c {high = c.high + 1})

incrRX :: State S ()
incrRX = modify (\s -> s {presses = s.presses + 1})

lookupModules :: [String] -> State S [Module]
lookupModules xs = gets ((\m -> mapMaybe (`Map.lookup` m) xs) . modMap)

updateModule :: String -> Module -> State S ()
updateModule str modul =
    modify (\s -> s {modMap = Map.insert str modul s.modMap})

parse :: String -> Map String Module
parse = Map.fromList . map (go . splitOn " -> ") . lines
  where
    go :: [String] -> (String, Module)
    go [name, receivers]
        | "%" `isPrefixOf` name =
            (tail name, FlipFlop (splitOn ", " receivers) Off)
        | "&" `isPrefixOf` name =
            (tail name, Conjunction (splitOn ", " receivers) mempty)
        | otherwise = (name, Broadcaster (splitOn ", " receivers))
    go _ = error "parsing failed"

allHigh :: Map String Pulse -> Bool
allHigh = all (== High) . Map.elems

addPrevs :: State S ()
addPrevs = do
    modify (\s -> s {modMap = go (Map.keys s.modMap) s.modMap})
  where
    go :: [String] -> Map String Module -> Map String Module
    go [] m = m
    go (x : xs) m = case Map.lookup x m of
        Just (Conjunction tos _) -> go xs (go2 tos x m)
        Just (FlipFlop tos _) -> go xs (go2 tos x m)
        Nothing -> error (show x)
        _ -> go xs m
      where
        go2 :: [String] -> String -> Map String Module -> Map String Module
        go2 [] _ m = m
        go2 (x : xs) neighbor m = case Map.lookup x m of
            Nothing -> go2 xs neighbor m
            Just (Conjunction tos nei) ->
                go2
                    xs
                    neighbor
                    ( Map.insert
                        x
                        (Conjunction tos (Map.insert neighbor Low nei))
                        m
                    )
            _ -> go2 xs neighbor m

step
    :: String
    -> String
    -> Pulse
    -> State S [(Pulse, String, String)]
step from to pulse = do
    increment pulse
    lookupModules [to] >>= \case
        [] -> pure []
        [Broadcaster tos] -> pure (map (pulse,to,) tos)
        [FlipFlop tos status] -> case pulse of
            High -> pure []
            Low -> do
                case status of
                    On ->
                        updateModule
                            to
                            (FlipFlop tos Off)
                            >> pure (map (Low,to,) tos)
                    Off ->
                        updateModule
                            to
                            (FlipFlop tos On)
                            >> pure (map (High,to,) tos)
        [Conjunction tos received'] -> do
            let received = Map.insert from pulse received'
            updateModule to (Conjunction tos received)
            rx <- gets presses
            prevs <- gets (snd . rxCon)
            if allHigh received
                then pure (map (Low,to,) tos)
                else do
                    when
                        (to `elem` prevs)
                        (modify (\s -> s {lcmMap = Map.insert to rx s.lcmMap}))
                    pure (map (High,to,) tos)
        _ -> error "unreachable"

steps :: State S ()
steps = go [(Low, "button", "broadcaster")]
  where
    go :: [(Pulse, String, String)] -> State S ()
    go [] = pure ()
    go ((pulse, from, to) : xs) = do
        ys <- step from to pulse
        go (xs ++ ys)

stepsN :: Int -> Int -> State S Int
stepsN from to
    | from == to = pure 0
    | otherwise = do
        incrRX
        steps
        modul <- gets modMap
        lcms <- gets lcmMap
        penultimate <- gets (fst . rxCon)
        case Map.lookup penultimate modul of
            Just (Conjunction _ prevs) -> do
                let keys = Map.keys prevs
                if all (`Map.member` lcms) keys
                    then pure (foldl1 lcm (Map.elems lcms))
                    else stepsN (from + 1) to
            Nothing -> error (show penultimate)
            _ -> stepsN (from + 1) to

run :: String -> State S a -> (a, S)
run s st = runState st (S 0 0 p 0 mempty ("", []))
  where
    p = parse s

con :: State S ()
con = do modify (\s -> s {rxCon = go s.modMap})
  where
    go :: Map String Module -> (String, [String])
    go m = go2 (Map.keys m)
      where
        go2 [] = error "Rx not found"
        go2 (x : xs) = case Map.lookup x m of
            Just modul ->
                if "rx" `elem` tos modul
                    then (x, previous modul)
                    else go2 xs
            Nothing -> error (show x)

p1 :: String -> Int
p1 s = mul (snd (run s (addPrevs >> con >> stepsN 0 1000)))
  where
    mul :: S -> Int
    mul s = s.low * s.high

p2 :: String -> Int
p2 s = fst (run s (addPrevs >> con >> stepsN 0 maxBound))

solve :: AOC
solve = AOC 20 p1 p2
