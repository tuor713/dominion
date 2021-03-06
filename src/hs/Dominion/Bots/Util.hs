module Dominion.Bots.Util where

import Dominion.Cards
import Dominion.Model

import qualified Control.Monad as M
import qualified Data.Map.Strict as Map
import qualified Data.IORef as IORef


-- TODO
-- => the bot returning a GameStep directly is not a very safe pattern
--    it presupposes bots are correctly implemented and not malicious
-- => should we include bot state or is this covered by IO monad ?
type Bot a = GameState -> (Decision (SimulationT a)) -> IO (SimulationT a)
type AIBot a = GameState -> (Decision (SimulationT a)) -> SimulationT a
type StateBot a b = a -> GameState -> (Decision (SimulationT b)) -> (SimulationT b,a)

-- Utilities for defining bots

type SimpleBot a = PlayerId -> GameState -> (Decision (SimulationT a)) -> Maybe (SimulationT a) -> SimulationT a
type PartialBot a = PlayerId -> GameState -> (Decision (SimulationT a)) -> Maybe (SimulationT a) -> Maybe (SimulationT a)

simpleBot :: SimpleBot a -> PlayerId -> AIBot a
simpleBot bot id state (Optional inner next) = bot id state inner (Just next)
simpleBot bot id state decision = bot id state decision Nothing

aiBot :: AIBot a -> Bot a
aiBot bot state int = return $ bot state int

stateBot :: a -> StateBot a b -> IO (Bot b)
stateBot initial bot = do
  ref <- IORef.newIORef initial
  return $ \state decision -> do
    s <- IORef.readIORef ref
    let (sim,s') = bot s state decision
    IORef.writeIORef ref s'
    return sim

-- Behavioural traits


alt :: PartialBot a -> PartialBot a -> PartialBot a
alt bot1 bot2 id state decision opt = bot1 id state decision opt `M.mplus` bot2 id state decision opt

buysC :: CardDef -> (GameState -> Bool) -> PartialBot a
buysC card pred _ state (ChooseCard (EffectBuy _) choices f) _
  | null cs || not (pred state) = Nothing
  | otherwise = Just $ f (head cs)
  where
    cs = filter ((==card) . typ) choices
buysC _ _ _ _ _ _ = Nothing

buys :: String -> PartialBot a
buys cardName = buysC (lookupCard cardName) (const True)

buysIf :: String -> (GameState -> Bool) -> PartialBot a
buysIf cardName pred = buysC (lookupCard cardName) pred


-- Utility Predicates

colonyGame :: GameState -> Bool
colonyGame state = Map.member colony (piles state)

moneyInHand :: PlayerId -> GameState -> Int
moneyInHand id state = moneySum $ hand $ playerByName state id

moneyInDeck :: PlayerId -> GameState -> Int
moneyInDeck id state = moneySum $ allCards $ playerByName state id

totalMoney :: GameState -> Int
totalMoney state = state |> activePlayer |> allCards |> map (moneyValue . typ) |> sum

gainsToEndGame :: GameState -> Int
gainsToEndGame state = min cardsToThreePile (min provinceLeft colonyLeft)
  where
    provinceLeft = numInSupply state province
    colonyLeft = if colonyGame state then numInSupply state colony else 100
    cardsToThreePile = min3 100 100 100 $ map length $ Map.elems $ piles state
    min3 a b c [] = a+b+c
    min3 a b c (x:xs)
      | x >= c = min3 a b c xs
      | x >= b = min3 a b x xs
      | x >= a = min3 a x b xs
      | otherwise = min3 x a b xs

deckSize :: GameState -> Int
deckSize state = length $ allCards $ activePlayer state

numInDeck :: String -> GameState -> Int
-- Leads to better caching, card is lookuped only once ...
numInDeck name = \state -> length $ filter ((==card) . typ) $ allCards $ activePlayer state
  where
    card = lookupCard name

numInDeckAndMats :: String -> GameState -> Int
numInDeckAndMats name = \state -> length $ filter ((==card) . typ) (let p = activePlayer state in allCards p ++ concat (Map.elems (mats p)))
  where
    card = lookupCard name

numTypeInDeck :: CardType -> GameState -> Int
numTypeInDeck typ = \state -> length $ filter (`hasCardType` typ) $ allCards $ activePlayer state
