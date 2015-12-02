{-# LANGUAGE TupleSections #-}
module Dominion where

import Dominion.Model
import Dominion.Cards
import Dominion.Bots
import Dominion.Stats

import qualified Control.Monad as M
import Prelude hiding (interact)
import qualified Data.List as L
import System.Random (StdGen, mkStdGen, randomR, newStdGen)
import Data.List.Split (wordsBy)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe

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

decisionType2message :: DecisionType -> String
decisionType2message QDraw = "Draw?"
decisionType2message QPlay = "Play a card?"
decisionType2message QTreasures = "Play treasures?"
decisionType2message QBuy = "Buy a card?"
decisionType2message QGain = "Gain a card?"
decisionType2message QTrash = "Trash a card?"
decisionType2message (QDiscard _) = "Discard a card?"
decisionType2message QOption = "Use effect?"

decision2prompt :: PlayerId -> Decision -> (String, String -> Maybe Simulation)
decision2prompt player (YesNo typ card f) = (message player (decisionType2message typ ++ " " ++ cardName card ++ " [yn]"), (fmap f) . parse)
  where
    parse "y" = Just True
    parse "n" = Just False
    parse _ = Nothing

decision2prompt player (Choice typ choices f) =
  (message player (decisionType2message typ ++ " " ++ summarizeCards choices), (fmap f) . parse)
  where
    parse s = maybeCard s >>= \x -> if x `elem` choices then Just x else Nothing

decision2prompt player (Choices typ choices (lo,hi) f) =
  (message player (decisionType2message typ ++ " " ++ summarizeCards choices), (fmap f) . parse)
  where
    parse s = if s == "all" then Just choices else (sequence $ map maybeCard (wordsBy (==',') s))
              >>= \xs -> if contains (list2MultiSet choices) (list2MultiSet xs) then Just xs else Nothing
              >>= \xs -> if lo <= length xs && length xs <= hi then Just xs else Nothing

decision2prompt player (Optional inner next) = (msg, parser)
  where
    (msg, handler) = decision2prompt player inner
    parser "" = Just next
    parser s = handler s

interact :: PlayerId -> Decision -> IO Simulation
interact player decision = putStrLn info >> getInput handler
  where
    (info, handler) = decision2prompt player decision

human :: PlayerId -> Bot
human player _ = interact player


-- Simulation running

runSimulation :: Map.Map PlayerId AIBot -> [GameState] -> GameStep -> SimulationT [GameState]
runSimulation bots accu (State state)
  | finished state = return $ reverse (state:accu)
  | otherwise =
    do
      next <- playTurn (activePlayerId state) state
      runSimulation bots (state:accu) next

runSimulation bots accu (Decision player state decision) =
  do
    next <- (bots Map.! player) (visibleState player state) decision
    runSimulation bots accu next

-- Sample run:
-- runSimulations [("Alice",bigSmithy), ("Bob",betterBigMoney)] (map lookupCard ["market", "library", "smithy", "cellar", "chapel", "witch", "village", "laboratory", "festival", "festival"]) 100 >>= stats
runSimulations :: [(PlayerId,AIBot)] -> [Card] -> Stats -> Int -> SimulationT Stats
runSimulations _ _ stats 0 = return stats
runSimulations bots tableau stats num =
  do
    players <- shuffle (map fst bots)
    initial <- mkGame StandardGame players tableau
    states <- runSimulation (Map.fromList bots) [] (State initial)
    runSimulations bots tableau (collectStats stats states) (num-1)


-- Stats

showStats :: Stats -> String
showStats stats =
  "No of games: " ++ show (statNumberOfGames stats) ++ "\n" ++
  "Wins ratios: " ++ show (statWinRatio stats) ++ "\n" ++
  "Length of games: " ++ show (statTurnsPerGame stats)

showInfos :: [Info] -> IO ()
showInfos infos = M.mapM_ (\(vis,msg) -> putStrLn ("[" ++ show vis ++ "] " ++ msg)) infos

runGameConsole :: Map.Map PlayerId Bot -> GameStep -> StdGen -> IO ()
runGameConsole bots (State state) gen
  | finished state =
    do
      putStrLn "Game Finished!"
      putStrLn $ "Final score: " ++ (players state
                                      |> Map.elems
                                      |> L.sortOn points
                                      |> reverse
                                      |> map (\p -> name p ++ ": " ++ show (points p))
                                      |> L.intersperse ", "
                                      |> concat)
  | otherwise =
    do
      let (next,(gen',infos)) = runSim (playTurn (activePlayerId state) state) gen
      showInfos infos
      runGameConsole bots next gen'

runGameConsole bots (Decision player state decision) gen =
  do
    sim <- (bots Map.! player) (visibleState player state) decision
    let (next,(gen',infos)) = runSim sim gen
    showInfos infos
    runGameConsole bots next gen'


{- Sample invocation (ghci)
   playOnConsole [("Alice",human "Alice"),("Bob",human "Bob")] ["cellar", "market", "militia", "mine", "moat", "remodel", "smithy", "village", "woodcutter", "workshop"]
   playOnConsole [("Alice",human "Alice"), ("Bob",betterBigMoney)] ["market", "library", "smithy", "laboratory", "village", "witch", "thief", "bureaucrat", "militia", "chapel"]
   playOnConsole [("Alice",basicBigMoney), ("Bob",betterBigMoney)] ["market", "library", "smithy"]
-}
playOnConsole :: [(String,Bot)] -> [String] -> IO ()
playOnConsole players cards =
  do
    gen <- newStdGen
    let (initial,(gen',_)) = runSim (mkGame StandardGame (map fst players) (map lookupCard cards)) gen
    runGameConsole (Map.fromList players) (State initial) gen'
