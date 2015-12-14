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
type Bot = GameState -> Decision -> IO Simulation
type AIBot = GameState -> Decision -> Simulation


-- Utilities for defining bots

type SimpleBot = PlayerId -> GameState -> Decision -> Maybe Simulation -> Simulation
type PartialBot = PlayerId -> GameState -> Decision -> Maybe Simulation -> Maybe Simulation

simpleBot :: SimpleBot -> PlayerId -> AIBot
simpleBot bot id state (Optional inner next) = bot id state inner (Just next)
simpleBot bot id state decision = bot id state decision Nothing

aiBot :: AIBot -> Bot
aiBot bot state int = return $ bot state int


-- Default AI

-- Default predicate to rate a card, not context sensitive
cardScore :: CardDef -> Int
cardScore card
  | card == curse = 0
  | card == estate = 5
  | card == copper = 10
  | card == silver = 50
  | card == gold = 100
  | card == cSmithy = 60
  | otherwise = 40

defaultBot :: SimpleBot
defaultBot _ _ (ChooseCards (EffectPlayTreasure _) choices _ f) _ = f choices
defaultBot _ _ (ChooseCard (EffectPlayAction _) actions f) _ = f (head actions)
defaultBot _ state (ChooseCard (EffectTrash _ _) cards f) alt
  | curse `elem` (map typ cards) = f $ head (filter ((==curse) . typ) cards)
  | estate `elem` (map typ cards) && gainsToEndGame state > 2 = f $ head (filter ((==estate) . typ) cards)
  | copper `elem` (map typ cards) = f $ head (filter ((==copper) . typ) cards)
  | otherwise = case alt of
                  Just next -> next
                  Nothing -> f (head cards)

defaultBot ownId _ (ChooseToUse (EffectDiscard card (TopOfDeck p)) f) _
  | ownId == p = f (not desirable)
  | otherwise = f desirable
  where
    ctyp = typ card
    desirable = ctyp /= copper && ctyp /= curse && ctyp /= estate && ctyp /= duchy && ctyp /= province

defaultBot _ _ (ChooseCards (EffectTrash _ _) cards (lo,hi) f) _ = f choices
  where
    wantsToTrash = filter (\c -> typ c == curse || typ c == copper || typ c == estate) cards
    trashOrder = L.sortOn (cardScore . typ) cards
    choices = take (min (max lo (length wantsToTrash)) hi) trashOrder

-- Default choice take the alternative
defaultBot _ _ _ (Just next) = next

-- Bad or blind choices
defaultBot pid _ (ChooseCards (EffectDiscard _ (Hand p)) choices (lo,hi) f) _
  | pid == p = f $ take lo $ L.sortOn (cardScore . typ) choices
  | otherwise = f $ take hi $ L.sortOn (negate . cardScore . typ) choices


defaultBot _ _ (ChooseToReact _ _ f) _ = f False
defaultBot _ _ (ChooseToUse _ f) _ = f False
defaultBot _ _ (ChooseCards _ choices (lo,_) f) _ = f (take lo choices)
defaultBot _ _ (ChooseCard _ choices f) _ = f (head choices)
defaultBot _ _ (ChooseEffects no effects f) _ = f (take no effects)
-- this is handled earlier ...
defaultBot _ _ (Optional _ alt) _ = alt


-- Behavioural traits

partialBot :: PartialBot -> PlayerId -> AIBot
partialBot bot id = simpleBot (\id state decision option -> Maybe.fromMaybe (defaultBot id state decision option) $ bot id state decision option) id

alt :: PartialBot -> PartialBot -> PartialBot
alt bot1 bot2 id state decision opt = bot1 id state decision opt `M.mplus` bot2 id state decision opt

buysC :: CardDef -> (GameState -> Bool) -> PartialBot
buysC card pred _ state (ChooseCard (EffectBuy _) choices f) _
  | null cs || not (pred state) = Nothing
  | otherwise = Just $ f (head cs)
  where
    cs = filter ((==card) . typ) choices
buysC _ _ _ _ _ _ = Nothing

buys :: String -> PartialBot
buys cardName = buysC (lookupCard cardName) (const True)

buysIf :: String -> (GameState -> Bool) -> PartialBot
buysIf cardName pred = buysC (lookupCard cardName) pred


-- Utility Predicates

colonyGame :: GameState -> Bool
colonyGame state = Map.member colony (piles state)

moneyValue :: CardDef -> Int
moneyValue card
  | card == gold = 3
  | card == silver = 2
  | card == copper = 1
  | otherwise = 0

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


-- Actual Bots - mostly taken from dominiate

basicBigMoney = partialBot $ buys "Province" `alt` buys "Gold" `alt` buys "Silver"

-- Penultimate province rule
checkPPR state =
  Map.size (players state) > 2
  || numInSupply state province > 2

  -- don't commit suicide by taking the last province,
  -- as second player take the drawn
  || (numInSupply state province == 1
      && ((firstPlayer && myPoints + 6 > opPoints)
          || (not firstPlayer && myPoints + 6 >= opPoints)))

  -- penultimate rule: there is a random turn restriction because
  -- after a certain amount of turns the opponent might not actually be that likely to be able
  -- to get the last province
  || (numInSupply state province == 2 && turnNo state >= 12)
  || (numInSupply state province == 2
      && ((firstPlayer && myPoints > opPoints)
          || (not firstPlayer && myPoints >= opPoints)))
  where
    me = activePlayer state
    myPoints = points $ me
    opPoints = points $ head $ opponents state (name me)
    firstPlayer = ply state `mod` 2 == 1


botLibrary :: [(String, PlayerId -> AIBot)]
botLibrary =
  [("Big Money", betterBigMoney),
   ("Big Money Smithy", bigSmithy),
   ("Double Jack", doubleJack),
   ("Double Militia", doubleMilitia),
   ("Chapel Witch", chapelWitch)]

betterBigMoney = partialBot $
  buysIf "Province" ((>15) . totalMoney)
  `alt` buysIf "Duchy" ((<=5) . gainsToEndGame)
  `alt` buysIf "Estate" ((<=2) . gainsToEndGame)
  `alt` buys "Gold"
  `alt` buys "Silver"

bigSmithy = partialBot $
  buysIf "Province" (\s -> totalMoney s > 15 && checkPPR s)
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

doubleMilitia = partialBot $
  buys "Province"
  `alt` buysIf "Duchy" ((<=5) . gainsToEndGame)
  `alt` buysIf "Militia" ((<2) . (numInDeck "Militia"))
  `alt` buysIf "Estate" ((<=2) . gainsToEndGame)
  `alt` buys "Gold"
  `alt` buys "Silver"

chapelWitch = partialBot $
  buys "Province"
  `alt` buysIf "Witch" ((==0) . (numInDeck "Witch"))
  `alt` buysIf "Duchy" ((<=5) . gainsToEndGame)
  `alt` buysIf "Estate" ((<=2) . gainsToEndGame)
  `alt` buys "Gold"
  `alt` buysIf "Chapel" (\s -> numInDeck "Chapel" s == 0)
  `alt` buys "Silver"

