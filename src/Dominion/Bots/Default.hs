module Dominion.Bots.Default where

import Dominion.Bots.Util
import Dominion.Cards
import Dominion.Model

import qualified Data.List as L


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

defaultBot _ _ (ChooseNumber _ (lo,_) f) _ = f lo
defaultBot _ _ (ChooseToReact _ _ f) _ = f False
defaultBot _ _ (ChooseToUse _ f) _ = f False
defaultBot _ _ (ChooseCards _ choices (lo,_) f) _ = f (take lo choices)
defaultBot _ _ (ChooseCard _ choices f) _ = f (head choices)
defaultBot _ _ (ChooseEffects no effects f) _ = f (take no effects)
-- this is handled earlier ...
defaultBot _ _ (Optional _ alt) _ = alt
