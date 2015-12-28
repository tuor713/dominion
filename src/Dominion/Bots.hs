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

import qualified Data.List as L

defaultBotId :: String
defaultBotId = "Multi Strategy"

altStrategy :: PlayerId -> (GameState -> Bool) -> (PlayerId -> AIBot) -> StateBot (Maybe AIBot) -> StateBot (Maybe AIBot)
altStrategy _ _ _ _ (Just bot) state decision = (bot state decision, Just bot)
altStrategy id pred bot fallback Nothing state decision
  | pred state = ((bot id) state decision, Just (bot id))
  | otherwise = fallback Nothing state decision

defaultStrategy :: AIBot -> StateBot (Maybe AIBot)
defaultStrategy _ (Just bot) state decision = (bot state decision, Just bot)
defaultStrategy bot Nothing state decision = (bot state decision, Just bot)

multiStrategy :: PlayerId -> StateBot (Maybe AIBot)
multiStrategy id =
  altStrategy id (\s -> cardInTableau (lookupCard "Beggar") s && cardInTableau (lookupCard "Gardens") s) beggarGardens $
  altStrategy id (cardInTableau (lookupCard "Courtyard")) bmCourtyard $
  altStrategy id (cardInTableau cJackOfAllTrades) doubleJack $
  altStrategy id (\s -> cardInTableau cChapel s && cardInTableau cWitch s) chapelWitch $
  altStrategy id (\s -> cardInTableau duchy s && cardInTableau (lookupCard "Duke") s) dukeDuchy $
  altStrategy id (cardInTableau cMilitia) doubleMilitia $
  altStrategy id (cardInTableau cSmithy) bigSmithy $
  altStrategy id (cardInTableau cLibrary) bigLibrary $
  defaultStrategy (betterBigMoney id)

botLibrary :: [(String, PlayerId -> IO Bot)]
botLibrary = L.sortOn fst $
  [("Big Money", return . aiBot . betterBigMoney),
   ("Big Smithy", return . aiBot . bigSmithy),
   ("Big Library", return . aiBot . bigLibrary),
   ("BM Courtyard", return . aiBot . bmCourtyard),
   ("Double Jack", return . aiBot . doubleJack),
   ("Double Militia", return . aiBot . doubleMilitia),
   ("Chapel Witch", return . aiBot . chapelWitch),
   ("Multi Strategy", stateBot Nothing . multiStrategy),
   ("Duke Duchy", return . aiBot . dukeDuchy),
   ("Ill-Gotten Gains Rush", return . aiBot . illGottenGainsRush),
   ("BeggarGardens", return . aiBot . beggarGardens)]

