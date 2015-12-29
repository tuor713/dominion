module Dominion.Bots.Default where

import Dominion.Bots.Util
import Dominion.Cards
import Dominion.Model

import qualified Data.List as L
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

data DefaultBotConfig =
  DefaultBotConfig {
    buyLevels :: !(Set.Set Int)
  }

emptyConfig :: DefaultBotConfig
emptyConfig = DefaultBotConfig { buyLevels = Set.fromList [2,3,4,5,6,7,8,9,11] }

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

stashScore :: CardDef -> Int
stashScore card
  | types card == [Victory] = 100
  | card == curse = 75
  | card == copper = 60
  | card == silver = 50
  | card == gold = 0
  | otherwise = 20

defaultBot :: DefaultBotConfig -> SimpleBot
defaultBot _ _ _ (ChooseCards (EffectPlayTreasure _) choices _ f) _ = f choices
defaultBot _ _ _ (ChooseCard (EffectPlayAction _) actions f) _ = f (head actions)
defaultBot _ _ state (ChooseCard (EffectTrash _ _) cards f) alt
  | curse `elem` (map typ cards) = f $ head (filter ((==curse) . typ) cards)
  | estate `elem` (map typ cards) && gainsToEndGame state > 2 = f $ head (filter ((==estate) . typ) cards)
  | copper `elem` (map typ cards) = f $ head (filter ((==copper) . typ) cards)
  | otherwise = case alt of
                  Just next -> next
                  Nothing -> f (head cards)

defaultBot config ownId state (ChooseCard (EffectPut _ (Hand p) (TopOfDeck _)) cards f) alt
  -- Courtyard type putting cards back
  | ownId == p && activePlayerId state == ownId && actions (turn state) == 0 && any isAction cards =
    f (L.maximumBy (\a b -> compare (cardScore (typ a)) (cardScore (typ b))) $ filter isAction cards)
  | ownId == p && activePlayerId state == ownId && Set.notMember (moneyInHand ownId state) (buyLevels config) =
    let money = moneyInHand ownId state
        nextLevel = Set.lookupLE money (buyLevels config)
    in
      case nextLevel of
        Just level ->
          let diff = money - level in
            (case filter ((<=diff) . moneyValue . typ) (L.sortOn ((0-) . moneyValue . typ) cards) of
              (a:_) -> f a
              [] -> fallback)
        Nothing -> fallback
  | ownId == p && activePlayerId state == ownId && Set.member (moneyInHand ownId state) (buyLevels config) =
    case (filter ((==0) . moneyValue . typ) cards) of
      (a:_) -> f a
      [] -> fallback
  | otherwise = fallback
  where
    fallback = Maybe.fromMaybe (f (head cards)) alt

defaultBot _ _ _ (ChooseCard (EffectPut _ (Hand _) (Mat _ IslandMat)) cards f) alt
  | stashScore (typ bestStash) > 50 = f bestStash
  | otherwise = case alt of
                  Just next -> next
                  Nothing -> f bestStash
  where
    bestStash = L.maximumBy (\c1 c2 -> compare (stashScore (typ c1)) (stashScore (typ c2))) cards

defaultBot _ ownId _ (ChooseToUse (EffectDiscard card (TopOfDeck p)) f) _
  | ownId == p = f (not desirable)
  | otherwise = f desirable
  where
    ctyp = typ card
    desirable = ctyp /= copper && ctyp /= curse && ctyp /= estate && ctyp /= duchy && ctyp /= province

defaultBot _ ownId state (ChooseCards (EffectTrash _ _) cards (lo,hi) f) _ = f choices
  where
    money = moneyInDeck ownId state
    mHand = moneySum cards
    wantsToTrash = filter (\c -> typ c == curse || (typ c == copper && (money - mHand <= 3)) || typ c == estate) cards
    trashOrder = L.sortOn (cardScore . typ) cards
    choices = take (min (max lo (length wantsToTrash)) hi) trashOrder

-- Default choice take the alternative
defaultBot _ _ _ _ (Just next) = next

-- Bad or blind choices
defaultBot _ pid _ (ChooseCards (EffectDiscard _ (Hand p)) choices (lo,hi) f) _
  | pid == p = f $ take lo $ L.sortOn (cardScore . typ) choices
  | otherwise = f $ take hi $ L.sortOn (negate . cardScore . typ) choices

defaultBot _ _ _ (ChooseNumber _ (lo,_) f) _ = f lo
defaultBot _ _ _ (ChooseToReact _ _ f) _ = f False
defaultBot _ _ _ (ChooseToUse _ f) _ = f False
defaultBot _ _ _ (ChooseCards _ choices (lo,_) f) _ = f (take lo choices)
defaultBot _ _ _ (ChooseCard _ choices f) _ = f (head choices)
defaultBot _ _ _ (ChooseEffects no effects f) _ = f (take no effects)
-- this is handled earlier ...
defaultBot _ _ _ (Optional _ alt) _ = alt
