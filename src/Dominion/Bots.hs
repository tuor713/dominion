module Dominion.Bots where

import Dominion.Model hiding (buys)
import Dominion.Cards

import qualified Control.Monad as M
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe

-- TODO
-- => the bot returning a GameStep directly is not a very safe pattern
--    it presupposes bots are correctly implemented and not malicious
-- => should we include bot state or is this covered by IO monad ?
type Bot = GameState -> Interaction -> IO GameStep


-- Utilities for defining bots

type SimpleBot = GameState -> Decision -> Maybe GameStep -> GameStep
type PartialBot = GameState -> Decision -> Maybe GameStep -> Maybe GameStep

simpleBot :: SimpleBot -> Bot
simpleBot bot state (Info _ next) = return next
simpleBot bot state (Decision (Optional inner next)) = return $ bot state inner (Just next)
simpleBot bot state (Decision decision) = return $ bot state decision Nothing

defaultBot :: SimpleBot
defaultBot _ (Choices QTreasures choices _ f) _ = f choices
defaultBot _ (Choice QPlay actions f) _ = f (head actions)
defaultBot _ (Choice QTrash cards f) alt
  | curse `elem` cards = f curse
  | estate `elem` cards = f estate
  | copper `elem` cards = f copper
  | otherwise = case alt of
                  Just next -> next
                  Nothing -> f (head cards)
defaultBot _ _ (Just next) = next
defaultBot _ (YesNo _ f) _ = f False
defaultBot _ (Choices _ choices val f) _ = f []
defaultBot _ (Choice _ choices f) _ = f (head choices)

partialBot :: PartialBot -> Bot
partialBot bot = simpleBot (\state decision option -> Maybe.fromMaybe (defaultBot state decision option) $ bot state decision option)

alt :: PartialBot -> PartialBot -> PartialBot
alt bot1 bot2 state decision opt = bot1 state decision opt `M.mplus` bot2 state decision opt


buysC :: Card -> (GameState -> Bool) -> PartialBot
buysC card pred state (Choice QBuy choices f) _
  | card `elem` choices && pred state = Just $ f card
  | otherwise = Nothing
buysC _ _ _ _ _ = Nothing

buys :: String -> PartialBot
buys cardName = buysC (lookupCard cardName) (const True)

buysIf :: String -> (GameState -> Bool) -> PartialBot
buysIf cardName pred = buysC (lookupCard cardName) pred


-- Utility Predicates

colonyGame :: GameState -> Bool
colonyGame state = Map.member colony (piles state)

moneyValue :: Card -> Int
moneyValue Card { cardName = "Gold" } = 3
moneyValue Card { cardName = "Silver" } = 2
moneyValue Card { cardName = "Copper" } = 1
moneyValue _ = 0

totalMoney :: GameState -> Int
totalMoney state = state |> activePlayer |> allCards |> map moneyValue |> sum

gainsToEndGame :: GameState -> Int
gainsToEndGame state = min cardsToThreePile (min provinceLeft colonyLeft)
  where
    provinceLeft = numInSupply state province
    colonyLeft = if colonyGame state then numInSupply state colony else 100
    cardsToThreePile = min3 100 100 100 $ Map.elems $ piles state
    min3 a b c [] = a+b+c
    min3 a b c (x:xs)
      | x >= c = min3 a b c xs
      | x >= b = min3 a b x xs
      | x >= a = min3 a x b xs
      | otherwise = min3 x a b xs

deckSize :: GameState -> Int
deckSize state = length $ allCards $ activePlayer state

numInDeck :: String -> GameState -> Int
numInDeck name state = length $ filter (==card) $ allCards $ activePlayer state
  where
    card = lookupCard name

-- Actual Bots

basicBigMoney = partialBot $ buys "Province" `alt` buys "Gold" `alt` buys "Silver"

-- Weights taken from dominate
betterBigMoney = partialBot $
  buysIf "Province" ((>15) . totalMoney)
  `alt` buysIf "Duchy" ((<=5) . gainsToEndGame)
  `alt` buysIf "Estate" ((<=2) . gainsToEndGame)
  `alt` buys "Gold"
  `alt` buys "Silver"

bigSmithy = partialBot $
  buysIf "Province" ((>15) . totalMoney)
  `alt` buysIf "Duchy" ((<=5) . gainsToEndGame)
  `alt` buysIf "Estate" ((<=2) . gainsToEndGame)
  `alt` buys "Gold"
  `alt` buysIf "Smithy" (\s -> let num = numInDeck "Smithy" s
                               in num < 1 || (num < 2 && deckSize s >= 16))
  `alt` buys "Silver"

doubleJack = partialBot $
  buysIf "Province" ((>15) . totalMoney)
  `alt` buysIf "Duchy" ((<=5) . gainsToEndGame)
  `alt` buysIf "Estate" ((<=2) . gainsToEndGame)
  `alt` buys "Gold"
  `alt` buysIf "Jack of All Trades" ((<2) . (numInDeck "Jack of All Trades"))
  `alt` buys "Silver"
