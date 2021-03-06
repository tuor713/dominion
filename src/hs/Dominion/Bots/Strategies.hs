module Dominion.Bots.Strategies where

import Dominion.Cards
import Dominion.Bots.Default
import Dominion.Bots.Util
import Dominion.Model hiding (buys)

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe



partialBotConfig :: DefaultBotConfig -> PartialBot a -> PlayerId -> AIBot a
partialBotConfig config bot id = simpleBot (\id state decision option -> Maybe.fromMaybe (defaultBot config id state decision option) $ bot id state decision option) id

partialBot :: PartialBot a -> PlayerId -> AIBot a
partialBot bot id = partialBotConfig emptyConfig bot id


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

bigLibrary = partialBot $
  buysIf "Province" (\s -> totalMoney s > 15 && checkPPR s)
  `alt` buysIf "Duchy" ((<=5) . gainsToEndGame)
  `alt` buysIf "Estate" ((<=2) . gainsToEndGame)
  `alt` buys "Gold"
  `alt` buysIf "Library" (\s -> let num = numInDeck "Library" s
                               in num < 1 || (num < 2 && deckSize s >= 16))
  `alt` buys "Silver"

bmMonument = partialBot $
  buysIf "Province" (\s -> totalMoney s > 15 && checkPPR s)
  `alt` buysIf "Duchy" ((<=5) . gainsToEndGame)
  `alt` buysIf "Estate" ((<=2) . gainsToEndGame)
  `alt` buys "Gold"
  `alt` buysIf "Monument" (\s -> let num = numInDeck "Monument" s
                                 in num < 1 || (num < 2 && deckSize s >= 16))
  `alt` buys "Silver"

bmCourtyard = partialBotConfig (emptyConfig { buyLevels = Set.fromList [2,3,6,8] }) $
  buysIf "Province" (\s -> totalMoney s > 15 && checkPPR s)
  `alt` buysIf "Duchy" ((<=4) . gainsToEndGame)
  `alt` buysIf "Estate" ((<=2) . gainsToEndGame)
  `alt` buys "Gold"
  `alt` buysIf "Duchy" ((<=5) . gainsToEndGame)
  `alt` buysIf "Silver" ((==0) . numInDeck "Silver")
  `alt` buysIf "Courtyard" ((==0) . numInDeck "Courtyard")
  `alt` buysIf "Courtyard" (\s -> (fromIntegral (numInDeck "Courtyard" s) :: Double)
                                  < (fromIntegral (numTypeInDeck Treasure s) / 8))
  `alt` buys "Silver"

bmWharf = partialBot $
  buysIf "Province" (\s -> totalMoney s > 15 && checkPPR s)
  `alt` buysIf "Duchy" ((<=5) . gainsToEndGame)
  `alt` buysIf "Estate" ((<=2) . gainsToEndGame)
  `alt` buys "Gold"
  `alt` buysIf "Wharf" (\s -> (fromIntegral (numInDeck "Wharf" s) :: Double)
                              < (fromIntegral (numTypeInDeck Treasure s) / 4))
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

-- Naive combo, no check if opponent is playing the mirror
beggarGardens = partialBot $
  buysIf "Gardens" (\s -> numInDeck "Beggar" s > 6 || numInSupply s cBeggar == 0)
  `alt` buys "Beggar"
  `alt` buys "Duchy"
  `alt` buysIf "Estate" (\s -> numInDeck "Gardens" s == 8 || numInSupply s cGardens == 0)
  `alt` buys "Silver"
  `alt` buys "Copper"

familiarOnly = partialBot $
  buys "Province"
  `alt` buysIf "Duchy" ((<=5) . gainsToEndGame)
  `alt` buysIf "Estate" ((<=2) . gainsToEndGame)
  `alt` buys "Gold"
  `alt` buys "Familiar"
  `alt` buysIf "Potion" ((<1) . (numInDeck "Potion"))
  `alt` buys "Silver"

alwaysGainCopper :: PartialBot a
alwaysGainCopper _ _ (ChooseToUse (EffectGain c (Hand _)) f) _
  | c == copper = Just (f True)
  | otherwise = Nothing
alwaysGainCopper _ _ _ _ = Nothing

illGottenGainsRush = partialBot $
  alwaysGainCopper
  `alt` buys "Province"
  `alt` buys "Ill-Gotten Gains"
  `alt` buys "Duchy"
  `alt` buys "Silver"

-- Naive, non-contested duke/duchy player
dukeDuchy = partialBot $
  buysIf "Gold" ((<=1) . (numInDeck "Gold"))
  `alt` buysIf "Duke" (\s -> (numInDeck "Duchy" s > 5 && numInDeck "Duke" s < numInDeck "Duchy" s)
                             || numInSupply s duchy == 0)

  `alt` buys "Duchy"
  `alt` buys "Silver"

islandSilkRoad = partialBot $
  buysIf "Silk Road" (\s -> numInDeckAndMats "Island" s >= 5 + numInDeckAndMats "Silk Road" s)
  `alt` buys "Island"
  `alt` buys "Silk Road"
  `alt` buys "Duchy"
  `alt` buysIf "Estate" ((<=4) . gainsToEndGame)
  `alt` buys "Silver"
  `alt` buys "Copper"
