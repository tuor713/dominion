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

altStrategy :: PlayerId -> (GameState -> Bool) -> (PlayerId -> AIBot a) -> StateBot (Maybe (AIBot a)) a -> StateBot (Maybe (AIBot a)) a
altStrategy _ _ _ _ (Just bot) state decision = (bot state decision, Just bot)
altStrategy id pred bot fallback Nothing state decision
  | pred state = ((bot id) state decision, Just (bot id))
  | otherwise = fallback Nothing state decision

defaultStrategy :: AIBot a -> StateBot (Maybe (AIBot a)) a
defaultStrategy _ (Just bot) state decision = (bot state decision, Just bot)
defaultStrategy bot Nothing state decision = (bot state decision, Just bot)

multiStrategy :: PlayerId -> StateBot (Maybe (AIBot a)) a
multiStrategy id =
  altStrategy id (\s -> cardInTableau (lookupCard "Beggar") s && cardInTableau (lookupCard "Gardens") s) beggarGardens $
  altStrategy id (cardInTableau (lookupCard "Wharf")) bmWharf $
  altStrategy id (cardInTableau (lookupCard "Courtyard")) bmCourtyard $
  altStrategy id (cardInTableau cJackOfAllTrades) doubleJack $
  altStrategy id (cardInTableau (lookupCard "Monument")) bmMonument $
  altStrategy id (\s -> cardInTableau cChapel s && cardInTableau cWitch s) chapelWitch $
  altStrategy id (\s -> cardInTableau duchy s && cardInTableau (lookupCard "Duke") s) dukeDuchy $
  altStrategy id (cardInTableau cMilitia) doubleMilitia $
  altStrategy id (cardInTableau cSmithy) bigSmithy $
  altStrategy id (cardInTableau cLibrary) bigLibrary $
  altStrategy id (cardInTableau (lookupCard "Familiar")) familiarOnly $
  defaultStrategy (betterBigMoney id)

botLibrary :: [(String, PlayerId -> IO (Bot a))]
botLibrary = L.sortOn fst $
  [("Big Money", return . aiBot . betterBigMoney),
   ("Big Smithy", return . aiBot . bigSmithy),
   ("Big Library", return . aiBot . bigLibrary),
   ("BM Courtyard", return . aiBot . bmCourtyard),
   ("BM Monument", return . aiBot . bmMonument),
   ("BM Wharf", return . aiBot . bmWharf),
   ("Island Silk Road", return . aiBot . islandSilkRoad),
   ("Double Jack", return . aiBot . doubleJack),
   ("Double Militia", return . aiBot . doubleMilitia),
   ("Chapel Witch", return . aiBot . chapelWitch),
   ("Familiar Only", return . aiBot . familiarOnly),
   ("Multi Strategy", stateBot Nothing . multiStrategy),
   ("Duke Duchy", return . aiBot . dukeDuchy),
   ("Ill-Gotten Gains Rush", return . aiBot . illGottenGainsRush),
   ("BeggarGardens", return . aiBot . beggarGardens)]

