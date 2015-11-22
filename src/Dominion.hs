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
decisionType2message QDiscard = "Discard a card?"
decisionType2message (QOption card) = "Use " ++ cardName card ++ "'s effect?"

decision2prompt :: PlayerId -> Decision -> (String, String -> Maybe GameStep)
decision2prompt player (YesNo typ f) = (message player (decisionType2message typ ++ " [yn]"), (fmap f) . parse)
  where
    parse "y" = Just True
    parse "n" = Just False
    parse _ = Nothing

decision2prompt player (Choice typ choices f) =
  (message player (decisionType2message typ ++ " " ++ summarizeCards choices), (fmap f) . parse)
  where
    parse s = lookupCard s >>= \x -> if x `elem` choices then Just x else Nothing

decision2prompt player (Choices typ choices validator f) =
  (message player (decisionType2message typ ++ " " ++ summarizeCards choices), (fmap f) . parse)
  where
    parse s = if s == "all" then Just choices else (sequence $ map lookupCard (wordsBy (==',') s))
              >>= \xs -> if contains (list2MultiSet choices) (list2MultiSet xs) then Just xs else Nothing
              >>= \xs -> if validator xs then Just xs else Nothing

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
  GameState { players = players,
              piles = map (\c -> replicate (noInPile c) c) (standardCards ++ kingdomCards),
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



runGameConsole :: Map.Map PlayerId Bot -> GameStep -> IO ()
runGameConsole bots (State state)
  | finished state =
    do
      putStrLn "Game Finished!"
      putStrLn $ "Final score: " ++ (players state
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
   playOnConsole [("Alice",human "Alice"),("Bob",human "Bob")] (map (Maybe.fromJust . lookupCard) ["cellar", "market", "militia", "mine", "moat", "remodel", "smithy", "village", "woodcutter", "workshop"])
   playOnConsole [("Alice",human "Alice"), ("Bob",bigMoneyBot Nothing)] (map (Maybe.fromJust . lookupCard) ["market", "library", "smithy", "laboratory", "village", "witch", "thief", "bureaucrat", "militia", "festival", "adventurer", "workshop", "feast", "gardens", "woodcutter", "chapel", "cellar", "moat", "mine", "chancellor", "moneylender", "remodel", "council room", "throne room", "spy"])
   playOnConsole [("Alice",bigMoneyBot Nothing), ("Bob",bigMoneyBot Nothing)] (map (Maybe.fromJust . lookupCard) ["market", "library", "smithy"])
-}
playOnConsole :: [(String,Bot)] -> [Card] -> IO ()
playOnConsole players cards =
  do
    gen <- newStdGen
    runGameConsole (Map.fromList players) $ State $ mkGame (map fst players) cards gen
