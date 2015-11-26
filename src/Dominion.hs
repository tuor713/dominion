{-# LANGUAGE TupleSections #-}
module Dominion where

import Dominion.Model
import Dominion.Cards
import Dominion.Bots

import Prelude hiding (interact)
import qualified Control.Monad.Trans.State.Lazy as St
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

decision2prompt :: PlayerId -> Decision -> (String, String -> Maybe GameStep)
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

interact :: PlayerId -> Interaction -> IO GameStep
interact player (Info info next) = putStrLn (message player info) >> return next
interact player (Decision decision) = putStrLn info >> getInput handler
  where
    (info, handler) = decision2prompt player decision

human :: PlayerId -> Bot
human player _ = interact player


mkPlayer :: String -> RandomState Player
mkPlayer name = draw Player { name = name, hand = [], discardPile = initialDeck, deck = [], inPlay = [] } 5

initialDeck :: [Card]
initialDeck = replicate 7 copper ++ replicate 3 estate

-- TODO card as data has made this less type safe
isStandardVictory c
  | c == estate = True
  | c == duchy = True
  | c == province = True
  | c == colony = True
  | otherwise = False

mkGame :: [String] -> [Card] -> StdGen -> GameState
mkGame names kingdomCards gen =
  GameState { players = Map.fromList $ zip names players,
              turnOrder = names,
              piles = Map.fromList $ map (\c -> (c, noInPile c)) (standardCards ++ kingdomCards),
              trashPile = [],
              turn = newTurn,
              ply = 1,
              gen = gen'
              }
  where
    standardCards = [estate,duchy,province,copper,silver,gold,curse]
    playerNo = length names
    noInPile card
      | isStandardVictory card && playerNo == 2 = 8
      | isStandardVictory card = 12
      | card == curse && playerNo == 2 = 10
      | card == curse && playerNo == 3 = 20
      | card == curse = 30
      | card == gold = 30
      | card == silver = 40
      | card == copper = 60 - 7 * playerNo
      | otherwise = 10

    (players,gen') = St.runState (sequence $ map mkPlayer names) gen


-- Simulation running

runSimulation :: Map.Map PlayerId AIBot -> [GameState] -> GameStep -> [GameState]
runSimulation bots accu (State state)
  | finished state = reverse (state:accu)
  | otherwise = runSimulation bots (state:accu) $ playTurn (activePlayerId state) state

runSimulation bots accu (Interaction player state (Info _ next)) =
  runSimulation bots accu next

runSimulation bots accu (Interaction player state interaction) = runSimulation bots accu next
  where
    next = (bots Map.! player) (visibleState player state) interaction

-- Sample run:
-- runSimulations [("Alice",bigSmithy), ("Bob",betterBigMoney)] (map lookupCard ["market", "library", "smithy", "cellar", "chapel", "witch", "village", "laboratory", "festival", "festival"]) 100 >>= stats
runSimulations :: [(PlayerId,AIBot)] -> [Card] -> Int -> StdGen -> [[GameState]]
runSimulations _ _ 0 _ = []
runSimulations bots tableau num ingen = states : runSimulations bots tableau (num-1) gen''
  where
    (players,gen') = shuffle' (map fst bots) ingen
    initial = mkGame players tableau gen'
    states = runSimulation (Map.fromList bots) [initial] $ State initial
    gen'' = gen $ last states


-- Stats

frequencies :: Ord a => [a] -> [(a,Int)]
frequencies xs = map (,1) xs |> Map.fromListWith (+) |> Map.toList |> L.sortOn (negate . snd)

winRatio games =
  games |>
  map last |>
  map winner |>
  Maybe.catMaybes |>
  frequencies |>
  map (\(p,num) -> (p, fromIntegral num / fromIntegral (length games)))
  where
    winner state
      | points one == points two = Just "Tie" -- Draw
      | otherwise = Just $ name one
      where
        (one:two:_) = players state |> Map.elems |> L.sortOn points |> reverse

stats :: [[GameState]] -> IO ()
stats games =
  do
    putStrLn $ "No of games: " ++ show (length games)
    putStrLn $ "Wins ratios: " ++ show (winRatio games)
    putStrLn $ "Length of games: " ++ show (L.sortOn fst $ frequencies (map (turnNo . last) games))


runGameConsole :: Map.Map PlayerId Bot -> GameStep -> IO ()
runGameConsole bots (State state)
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
      putStrLn ("Turn " ++ (show (turnNo state)) ++ ", player " ++ activePlayerId state)
      runGameConsole bots $ playTurn (activePlayerId state) state

runGameConsole bots (Interaction player state (Info msg next))
  | player == allPlayerId =
    do
      putStrLn (message "All" msg)
      runGameConsole bots next
  | otherwise =
    do
      next <- (bots Map.! player) (visibleState player state) (Info msg next)
      runGameConsole bots next

runGameConsole bots (Interaction player state decision@(Decision _)) =
  do
    next <- (bots Map.! player) (visibleState player state) decision
    runGameConsole bots next


{- Sample invocation (ghci)
   playOnConsole [("Alice",human "Alice"),("Bob",human "Bob")] ["cellar", "market", "militia", "mine", "moat", "remodel", "smithy", "village", "woodcutter", "workshop"]
   playOnConsole [("Alice",human "Alice"), ("Bob",betterBigMoney)] ["market", "library", "smithy", "laboratory", "village", "witch", "thief", "bureaucrat", "militia", "chapel"]
   playOnConsole [("Alice",basicBigMoney), ("Bob",betterBigMoney)] ["market", "library", "smithy"]
-}
playOnConsole :: [(String,Bot)] -> [String] -> IO ()
playOnConsole players cards =
  do
    gen <- newStdGen
    runGameConsole (Map.fromList players) $ State $ mkGame (map fst players) (map lookupCard cards) gen
