module Dominion.Bots
  (module Dominion.Bots.Util,
   module Dominion.Bots.Strategies,
   botLibrary,
   defaultBotId)
where

import Dominion.Bots.Strategies
import Dominion.Bots.Util

import Dominion.Model
import Dominion.Cards

defaultBotId :: String
defaultBotId = "Multi Strategy"

multiStrategy :: PlayerId -> StateBot (Maybe AIBot)
multiStrategy id Nothing state decision
  | cardInTableau cJackOfAllTrades state = use doubleJack
  | cardInTableau cChapel state && cardInTableau cWitch state = use chapelWitch
  | cardInTableau cMilitia state = use doubleMilitia
  | cardInTableau cSmithy state = use bigSmithy
  | otherwise = use betterBigMoney
  where
    use strat = multiStrategy id (Just (strat id)) state decision

multiStrategy _ (Just bot) state decision = (bot state decision, Just bot)

botLibrary :: [(String, PlayerId -> IO Bot)]
botLibrary =
  [("Big Money", return . aiBot . betterBigMoney),
   ("Big Money Smithy", return . aiBot . bigSmithy),
   ("Double Jack", return . aiBot . doubleJack),
   ("Double Militia", return . aiBot . doubleMilitia),
   ("Chapel Witch", return . aiBot . chapelWitch),
   ("Multi Strategy", stateBot Nothing . multiStrategy)]

