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
import System.Random (StdGen, newStdGen)
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

decision2prompt :: PlayerId -> Decision -> (String, String -> Maybe Simulation)
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
runSimulations :: [(PlayerId,AIBot)] -> [CardDef] -> Stats -> Int -> SimulationT Stats
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
