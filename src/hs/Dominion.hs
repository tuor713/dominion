{-# LANGUAGE ScopedTypeVariables, TupleSections #-}
module Dominion where

import Dominion.Model
import Dominion.Cards
import Dominion.Bots
import Dominion.Stats

import qualified Control.Monad as M
import Prelude hiding (interact)
import qualified Data.ByteString.Char8 as C8
import qualified Data.List as L
import System.Random (newStdGen)
import Data.List.Split (wordsBy)
import qualified Data.Map.Strict as Map

list2MultiSet :: Ord a => [a] -> Map.Map a Int
list2MultiSet xs = Map.fromListWith (+) $ map (,1) xs

contains :: Ord a => Map.Map a Int -> Map.Map a Int -> Bool
contains bigger smaller = Map.isSubmapOfBy (<=) smaller bigger

message :: String -> String -> String
message player msg = "["++player++"] "++ msg

getInput :: (String -> Maybe a) -> IO a
getInput parser =
  do
    putStr ">> "
    line <- getLine
    case parser line of
      Just v -> return v
      Nothing -> getInput parser

parseBool :: String -> Maybe Bool
parseBool "y" = Just True
parseBool "n" = Just False
parseBool _ = Nothing

findCard :: [Card] -> CardDef -> Maybe Card
findCard cards def
  | null cs = Nothing
  | otherwise = Just (head cs)
  where
    cs = filter ((==def) . typ) cards

findCards :: [Card] -> [CardDef] -> Maybe [Card]
findCards _ [] = Just []
findCards [] _ = Nothing
findCards cs (d:ds) = headCard >>= \c -> fmap (c:) (findCards (L.delete c cs) ds)
  where
    headCard = L.find ((==d) . typ) cs

decision2prompt :: PlayerId -> Decision (SimulationT a) -> (String, String -> Maybe (SimulationT a))
decision2prompt player (ChooseToUse effect f) = (message player (show effect ++ " [yn]"), fmap f . parseBool)

decision2prompt player (ChooseNumber effect (lo,hi) f) =
  (message player (show effect ++ " " ++ show lo ++ " up to " ++ show hi),
   \s -> fmap (f . fst) (C8.readInt (C8.pack s)))

decision2prompt player (ChooseCard effect choices f) =
  (message player (show effect ++ " " ++ summarizeCards choices),
   \s -> maybeCard s >>= findCard choices >>= (return . f))

decision2prompt player (ChooseCards effect choices (lo,hi) f) =
  (message player (show effect ++ " " ++ summarizeCards choices),
   \s -> parse s >>= (\xs -> findCards choices xs) >>= (return . f))
  where
    parse s = if s == "all" then Just (map typ choices) else (sequence $ map maybeCard (wordsBy (==',') s))
              >>= \xs -> if contains (list2MultiSet (map typ choices)) (list2MultiSet xs) then Just xs else Nothing
              >>= \xs -> if lo <= length xs && length xs <= hi then Just xs else Nothing

decision2prompt player (ChooseToReact card trigger f) =
  (message player ("Choose to react (" ++ show card ++ ") to " ++ show trigger),
   (fmap f) . parseBool)

decision2prompt player (ChooseEffects no effects f) =
  (message player ("Choose " ++ show no ++ ": " ++ (concat $ map (\(no::Int,effect) -> "[" ++ show no ++ "] " ++ show effect) $ zip [1..] effects)),
   (fmap f) . parse)
  where
    parse s =
      fmap (map (effects!!))
      $ M.mfilter (all (\x -> 0 <= x && x < length effects))
      $ fmap (map (+ (-1)))
      $ M.mfilter ((==no) . length . L.nub)
      $ M.mfilter ((==no) . length)
      $ fmap (map fst)
      $ (sequence (map (C8.readInt . C8.pack) (wordsBy (==',') s)))

decision2prompt player (Optional inner next) = (msg, parser)
  where
    (msg, handler) = decision2prompt player inner
    parser "" = Just next
    parser s = handler s

interact :: PlayerId -> Decision (SimulationT a) -> IO (SimulationT a)
interact player decision = putStrLn info >> getInput handler
  where
    (info, handler) = decision2prompt player decision

human :: PlayerId -> Bot a
human player _ = interact player


-- Simulation running

launchGame :: (a -> PlayerId -> GameState -> (Decision (SimulationT GameState)) -> (a -> SimulationT GameState -> IO b) -> IO b)
              -> (a -> GameState -> IO b)
              -> SimulationT GameState
              -> SimulationState
              -> a
              -> IO b
launchGame handler finishedHandler initial simState st =
  iter simState' st (playTurn (activePlayerId state) >> gameState')
  where
    (Result state,simState') = runSim initial simState
    iter simState st sim =
      case step of
        Result state ->
          if finished state
          then finishedHandler st state
          else iter simState'' st (playTurn (activePlayerId state) >> gameState')
        Decision pid state decision -> handler st pid state decision (iter simState'')
      where
        (step,simState'') = runSim sim simState


runSimulation :: Map.Map PlayerId (Bot GameState) -> SimulationT GameState -> SimulationState -> IO GameState
runSimulation bots initial state = launchGame onDecision (\_ final -> return final) initial state ()
  where
    onDecision _ player state decision cont =
      (bots Map.! player) (visibleState player state) decision >>= cont ()

runSimulations :: [(PlayerId,(Bot GameState))] -> [CardDef] -> Stats -> Int -> IO Stats
runSimulations _ _ stats 0 = return stats
runSimulations bots tableau stats num =
  do
    gen <- newStdGen
    let players = evalGameT (shuffle (map fst bots)) (seedSim gen)
    let initial = mkGame StandardGame players tableau
    final <- runSimulation (Map.fromList bots) initial (seedSim gen)
    runSimulations bots tableau (collectStats stats final) (num-1)

runSimulationsR :: [(PlayerId,(Bot GameState))] -> Stats -> Int -> IO Stats
runSimulationsR _ stats 0 = return stats
runSimulationsR bots stats num =
  do
    gen <- newStdGen
    let cardlist = evalGameT (shuffle kingdomCards) (seedSim gen)
    let players = evalGameT (shuffle (map fst bots)) (seedSim gen)
    let initial = mkGame StandardGame players (take 10 cardlist)
    final <- runSimulation (Map.fromList bots) initial (seedSim gen)
    runSimulationsR bots (collectStats stats final) (num-1)

runSampleGame :: [(PlayerId,Bot GameState)] -> [CardDef] -> IO GameState
runSampleGame bots tableau =
  do
    gen <- newStdGen
    let players = evalGameT (shuffle (map fst bots)) (seedSim gen)
    let initial = mkGame StandardGame players tableau
    runSimulation (Map.fromList bots) initial (seedSim gen)

-- Stats

showLogs :: [Log] -> IO ()
showLogs logs = M.forM_ logs (putStrLn . show)

showStats :: Stats -> String
showStats stats =
  "No of games: " ++ show (statNumberOfGames stats) ++ "\n" ++
  "Wins ratios: " ++ show (statWinRatio stats) ++ "\n" ++
  "Length of games: " ++ show (statTurnsPerGame stats)

playOnConsole :: [(String,Bot GameState)] -> [String] -> IO ()
playOnConsole ps cards =
  do
    let initial = mkGame StandardGame (map fst ps) (map lookupCard cards)
    gen <- newStdGen
    launchGame decisionHandler finishHandler initial (seedSim gen) 0
  where
    bots = Map.fromList ps
    decisionHandler num player state decision cont = do
      showLogs (drop num logs)
      sim <- (bots Map.! player) (visibleState player state) decision
      cont (length logs) sim
      where
        logs = gameLogs state

    finishHandler num state = do
      showLogs (drop num (gameLogs state))
      putStrLn "Game Finished!"
      putStrLn $ "Final score: " ++ (players state
                                      |> Map.elems
                                      |> L.sortOn points
                                      |> reverse
                                      |> map (\p -> name p ++ ": " ++ show (points p))
                                      |> L.intersperse ", "
                                      |> concat)
