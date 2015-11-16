{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Dominion where

import Dominion.Model
import Dominion.Cards

import Prelude hiding (interact)
import qualified Control.Monad as M
import qualified Control.Monad.Trans.State.Lazy as St
import qualified Data.List as L
import Data.List.Split (wordsBy)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import System.Random (StdGen, mkStdGen, randomR, newStdGen)
import Text.Read (readMaybe)


{-

== Modelling decisions ==
- how are triggers / reactions going to work
- what about global effects like Bridge
- model partial visibility
- do cards have an identity? (probably yes)
- should we model reveal ?

== Feature set ==
- non IO decision making like a bot

== Debt ==
- separate out random state completely
- make cards more data driven than all lengthy pattern matching
- separate domain model and card data / behaviour

-}


summarizeCards :: [Card] -> String
summarizeCards cards =
  cards |>
  L.sort |>
  L.group |>
  L.map (\cs -> (if (length cs > 1) then show (length cs) ++ "x " else "") ++ show (head cs)) |>
  L.intersperse ", " |>
  concat

instance Show GameState where
  show g = "Game {\n" ++
    "  players: {\n" ++
    concatMap
      (\p ->
        "    " ++ name p ++ ": {\n" ++
        "      hand:    [ " ++ summarizeCards (hand p) ++ " ]\n" ++
        "      play:    [ " ++ summarizeCards (inPlay p) ++ " ]\n" ++
        "      deck:    [ " ++ summarizeCards (deck p) ++ " ]\n" ++
        "      discard: [ " ++ summarizeCards (discardPile p) ++ " ]\n" ++
        "      points:  " ++ show (points p) ++ "\n" ++
        "    }\n")
      (players g) ++
    "  }\n" ++
    "  supply: [ " ++
    (summarizeCards $ concat (piles g)) ++
    " ]\n" ++
    "  turn: { " ++
    "actions: " ++ show (actions (turn g)) ++ ", buys: " ++ show (buys (turn g)) ++ ", money: " ++ show (money (turn g)) ++
    " }\n" ++
    "}"

-- Utilities


list2MultiSet :: Ord a => [a] -> Map.Map a Int
list2MultiSet xs = Map.fromListWith (+) $ map (,1) xs

contains :: Ord a => Map.Map a Int -> Map.Map a Int -> Bool
contains bigger smaller = Map.isSubmapOfBy (<=) smaller bigger


-- Basic operations

mainloop :: GameState -> IO ()
mainloop state =
  do
    next <- step state
    -- no actions means end of game
    if null (stack (turn next))
    then do
          putStrLn "Game Finished!"
          putStrLn $ "Final score: " ++ (players next
                                         |> L.sortOn points
                                         |> reverse
                                         |> map (\p -> name p ++ ": " ++ show (points p))
                                         |> L.intersperse ", "
                                         |> concat)
    else mainloop next

{- Sample invocation (ghci)
  playOnConsole ["Alice", "Bob"] (map (Maybe.fromJust . lookupCard) ["cellar", "market", "militia", "mine", "moat", "remodel", "smithy", "village", "woodcutter", "workshop"])
-}
playOnConsole :: [String] -> [Card] -> IO ()
playOnConsole players cards =
  do
    gen <- newStdGen
    mainloop $ mkGame players cards gen

step :: GameState -> IO GameState
step g =
  case stack (turn g) of
    (Left decision:steps) -> do putStrLn (show g)
                                newSteps <- interact decision
                                return $ g { turn = (turn g) { stack = (newSteps ++ steps) } }
    (Right action:steps) -> return $ execute action g { turn = (turn g) { stack = steps } }
    _ -> error "Illegal state, no steps but game has not ended"

getInput :: String -> (String -> Maybe a) -> IO a
getInput caption parser =
  do
    putStrLn caption
    putStr ">> "
    line <- getLine
    case parser line of
      Just v -> return v
      Nothing -> getInput caption parser


decision2prompt :: Interaction -> (String, String -> Maybe GameStack)
decision2prompt (PickACard player caption choices cont) =
  ("[" ++ player ++ "] " ++ caption ++ summarizeCards choices,
   (fmap cont) . (\s -> lookupCard s >>= \x -> if x `elem` choices then Just x else Nothing))

decision2prompt (PickCards player caption choices max cont) =
  ("[" ++ player ++ "] " ++ caption ++ summarizeCards choices,
   (fmap cont) . (\s -> if s == "all" then Just choices else (sequence $ map lookupCard (wordsBy (==',') s))
                        >>= \xs -> if length xs <= max then Just xs else Nothing
                        >>= \xs -> if contains (list2MultiSet choices) (list2MultiSet xs) then Just xs else Nothing))

decision2prompt (OptionalDecision decision) =
  (caption, handler')
  where
    (caption, handler) = decision2prompt decision
    handler' s = if s == "" then Just [] else handler s

decision2prompt (YesNoDecision player caption cont) =
  ("[" ++ player ++ "] " ++ caption ++ " [yn]", parse)
  where
    parse "y" = Just (cont True)
    parse "n" = Just (cont False)
    parse _ = Nothing

-- TODO this is partial because we don't support Information
decision2prompt (Information _ _) = undefined

interact (Information player message) =
  do putStrLn ("[" ++ player ++ "] " ++ message)
     return []

interact decision = getInput caption handler
  where
    (caption, handler) = decision2prompt decision


-- Some more model elements

mkPlayer :: String -> RandomState Player
mkPlayer name = draw Player { name = name, hand = [], discardPile = initialDeck, deck = [], inPlay = [] } 5

initialDeck :: [Card]
initialDeck = replicate 7 copper ++ replicate 3 estate

mkGame :: [String] -> [Card] -> StdGen -> GameState
mkGame names kingdomCards gen =
  GameState { players = players,
              piles = map (\c -> replicate (noInPile c) c) (standardCards ++ kingdomCards),
              trash = [],
              turn = newTurn,
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
