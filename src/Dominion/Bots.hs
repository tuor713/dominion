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

type SimpleBot = PlayerId -> GameState -> Decision -> Maybe GameStep -> GameStep
type PartialBot = PlayerId -> GameState -> Decision -> Maybe GameStep -> Maybe GameStep

simpleBot :: SimpleBot -> PlayerId -> Bot
simpleBot bot id state (Info _ next) = return next
simpleBot bot id state (Decision (Optional inner next)) = return $ bot id state inner (Just next)
simpleBot bot id state (Decision decision) = return $ bot id state decision Nothing

defaultBot :: SimpleBot
defaultBot _ _ (Choices QTreasures choices _ f) _ = f choices
defaultBot _ _ (Choice QPlay actions f) _ = f (head actions)
defaultBot _ state (Choice QTrash cards f) alt
  | curse `elem` cards = f curse
  | estate `elem` cards && gainsToEndGame state > 2 = f estate
  | copper `elem` cards = f copper
  | otherwise = case alt of
                  Just next -> next
                  Nothing -> f (head cards)

defaultBot ownId _ (YesNo (QDiscard (TopOfDeck p)) card f) _
  | ownId == p = f (not desirable)
  | otherwise = f desirable
  where
    desirable = card /= copper && card /= curse && card /= estate && card /= duchy && card /= province

defaultBot _ _ _ (Just next) = next
defaultBot _ _ (YesNo _ _ f) _ = f False
defaultBot _ _ (Choices _ choices val f) _ = f []
defaultBot _ _ (Choice _ choices f) _ = f (head choices)

partialBot :: PartialBot -> PlayerId -> Bot
partialBot bot id = simpleBot (\id state decision option -> Maybe.fromMaybe (defaultBot id state decision option) $ bot id state decision option) id

alt :: PartialBot -> PartialBot -> PartialBot
alt bot1 bot2 id state decision opt = bot1 id state decision opt `M.mplus` bot2 id state decision opt


buysC :: Card -> (GameState -> Bool) -> PartialBot
buysC card pred _ state (Choice QBuy choices f) _
  | card `elem` choices && pred state = Just $ f card
  | otherwise = Nothing
buysC _ _ _ _ _ _ = Nothing

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
