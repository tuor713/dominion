module Dominion.Cards
  (baseCards,
   prosperityCards,
   cardData,
   lookupCard,

   copper,
   curse,
   duchy,
   estate,
   gold,
   province,
   silver,
   (|>))
where

-- This module has the card knowledge base

import Dominion.Model

import qualified Control.Monad.Trans.State.Lazy as St
import Data.Char (toLower)
import qualified Data.Maybe as Maybe
import qualified Data.Map.Strict as Map
import qualified Data.List as L

-- Clojure / F# style threading
(|>) :: a -> (a -> b) -> b
(|>) x f = f x


type GameAction = GameState -> String -> GameState

pass :: GameAction
pass state _ = state

plusMoney :: Int -> GameAction
plusMoney num state _ = queueActions state [GainGold num]

plusCards :: Int -> GameAction
plusCards num state player = queueActions state [Draw player num]

plusActions :: Int -> GameAction
plusActions num state _ = queueActions state [GainActions num]

plusBuys :: Int -> GameAction
plusBuys num state _ = queueActions state [GainBuys num]

(&) :: GameAction -> GameAction -> GameAction
(&) act1 act2 state player = act2 (act1 state player) player

noPoints = const 0

noTriggers :: GameState -> String -> ActionStep -> GameStack
noTriggers _ _ _ = []


-- order is name, edition, cost, types, points, onPlay
baseCards =
  [Card "Estate" Base 2 [Victory] (const 1) pass noTriggers,
   Card "Duchy" Base 5 [Victory] (const 3) pass noTriggers,
   Card "Province" Base 8 [Victory] (const 6) pass noTriggers,
   Card "Curse" Base 0 [CurseType] (const (-1)) pass noTriggers,

   Card "Copper" Base 0 [Treasure] noPoints (plusMoney 1) noTriggers,
   Card "Silver" Base 3 [Treasure] noPoints (plusMoney 2) noTriggers,
   Card "Gold" Base 6 [Treasure] noPoints (plusMoney 3) noTriggers,


   Card "Cellar" Base 2 [Action] noPoints playCellar noTriggers,
   Card "Chapel" Base 2 [Action] noPoints playChapel noTriggers,
   Card "Moat" Base 2 [Action, Reaction] noPoints (plusCards 2) moatTrigger,

   Card "Chancellor" Base 3 [Action] noPoints playChancellor noTriggers,
   Card "Village" Base 3 [Action] noPoints (plusActions 2 & plusCards 1) noTriggers,
   Card "Woodcutter" Base 3 [Action] noPoints (plusBuys 1 & plusMoney 2) noTriggers,
   Card "Workshop" Base 3 [Action] noPoints playWorkshop noTriggers,

   Card "Bureaucrat" Base 4 [Action, Attack] noPoints playBureaucrat noTriggers,
   Card "Feast" Base 4 [Action] noPoints playFeast noTriggers,
   Card "Gardens" Base 4 [Victory] (\p -> length (allCards p) `quot` 10) pass noTriggers,
   Card "Militia" Base 4 [Action, Attack] noPoints playMilitia noTriggers,
   Card "Moneylender" Base 4 [Action] noPoints playMoneylender noTriggers,
   Card "Remodel" Base 4 [Action] noPoints playRemodel noTriggers,
   Card "Smithy" Base 4 [Action] noPoints (plusCards 3) noTriggers,
   Card "Spy" Base 4 [Action, Attack] noPoints playSpy noTriggers,
   Card "Thief" Base 4 [Action, Attack] noPoints playThief noTriggers,
   Card "Throne Room" Base 4 [Action] noPoints playThroneRoom noTriggers,

   Card "Council Room" Base 5 [Action] noPoints playCouncilRoom noTriggers,
   Card "Festival" Base 5 [Action] noPoints (plusActions 2 & plusBuys 1 & plusMoney 2) noTriggers,
   Card "Laboratory" Base 5 [Action] noPoints (plusCards 2 & plusActions 1) noTriggers,
   Card "Library" Base 5 [Action] noPoints playLibrary noTriggers,
   Card "Market" Base 5 [Action] noPoints (plusCards 1 & plusActions 1 & plusBuys 1 & plusMoney 1) noTriggers,
   Card "Mine" Base 5 [Action] noPoints playMine noTriggers,
   Card "Witch" Base 5 [Action, Attack] noPoints playWitch noTriggers,

   Card "Adventurer" Base 6 [Action] noPoints playAdventurer noTriggers
   ]

prosperityCards =
  [Card "Colony" Prosperity 11 [Victory] (const 10) pass noTriggers,
   Card "Platinum" Prosperity 9 [Treasure] noPoints (plusMoney 5) noTriggers]


cardData :: Map.Map String Card
cardData = Map.fromList $ map (\c -> (map toLower $ cardName c, c)) (baseCards ++ prosperityCards)


copper = cardData Map.! "copper"
curse = cardData Map.! "curse"
duchy = cardData Map.! "duchy"
estate = cardData Map.! "estate"
feast = cardData Map.! "feast"
province = cardData Map.! "province"
silver = cardData Map.! "silver"
gold = cardData Map.! "gold"

lookupCard :: String -> Maybe Card
lookupCard name = Map.lookup (map toLower name) cardData

playCellar g p =
  queueSteps g [Right (GainActions 1),
                Left (PickCards p
                                ("Choose any number of cards to discard: ")
                                (hand (playerByName g p))
                                (length (hand (playerByName g p)))
                                cont)]
  where
    cont cards = map Right $ map (DiscardCard p) cards ++ [Draw p (length cards)]

playChapel g p = queueSteps g [Left (PickCards p "Choose up to 4 cards to trash: " (hand (playerByName g p)) 4 cont)]
  where
    cont cards = map (Right . TrashCard p) cards

-- Do actions have identities that we could use to make moat apply?
moatDecision player card = Left $ YesNoDecision player "Use Moat's ability to prevent the attack?" cont
  where
    cont True = [Right $Branch rewriteStack]
    cont False = []
    rewriteStack g = g { turn = (turn g) { stack = iter g $ stack $ turn g } }
    iter _ [] = []
    iter g (Right (PlayCard card):xs) = Right (Branch hideplayer) : Right (PlayCard card) : Right (Branch unhideplayer) : xs
    iter g (Right (PlayCopy card):xs) = Right (Branch hideplayer) : Right (PlayCopy card) : Right (Branch unhideplayer) : xs
    iter g (x:xs) = x:iter g xs
    hideplayer g = updatePlayer g player (\p -> p { moated = True })
    unhideplayer g = updatePlayer g player (\p -> p { moated = False })


moatTrigger :: GameState -> String -> ActionStep -> GameStack
moatTrigger game player (PlayCard card)
  | name (activePlayer game) /= player && isAttack card = [moatDecision player card]
  | otherwise = []

moatTrigger game player (PlayCopy card)
  | name (activePlayer game) /= player && isAttack card = [moatDecision player card]
  | otherwise = []

moatTrigger _ _ _ = []

playChancellor g p = queueSteps g [Right $ GainGold 2,
                                    Left $ (YesNoDecision p "Put deck into discard pile?"
                                                          (\b -> if b then [Right $ Branch deckToDiscard] else []))]
  where
    -- Note that this is correct in as far as cards are not discarded individually
    deckToDiscard g = updatePlayer g p (\player -> player { deck = [], discardPile = deck player ++ discardPile player })

playWorkshop g p = queueSteps g [Left (PickACard p "Choose a card to gain: " (filter ((<=4) . cost) (availableCards g)) cont)]
  where
    cont card = [Right (GainCard (Discard p) card)]

playBureaucrat g p = queueSteps g $ [Right $ GainCard (TopOfDeck p) silver] ++ opDecisions
  where
    opDecisions = opponents g p
                  |> filter (not . null . filter isTreasure . hand)
                  |> map (\p -> Left $ PickACard (name p) "Choose a treasure to put on your deck: " (filter isTreasure (hand p))
                                       (\c -> [Right $ TransferCard c (Hand (name p)) (TopOfDeck (name p))]))

playFeast g p = queueSteps g' [Left (PickACard p "Choose a card to gain: " (filter ((<=5) . cost) (availableCards g')) cont)]
  where
    cont card = [Right (GainCard (TopOfDeck p) card)]
    g' = updatePlayer g p (\player -> player { inPlay = L.delete feast (inPlay player) })

playMilitia g p = queueSteps g ([Right (GainGold 2)]
                                 ++ Maybe.catMaybes (map (decision g) (opponentNames g p)))
  where
    decision g op = if length h <= 3
                    then Nothing
                    else Just $ Left $ PickCards p "Choose cards to discard:" h (length h - 3) (cont op)
      where
        h = hand (playerByName g op)
    cont op cards = map (Right . DiscardCard op) cards

-- TODO the linkage is imperfect because reactions and replacement effects could still happen
playMoneylender g p
  | copper `elem` h = queueActions g [TrashCard p copper, GainGold 3]
  | otherwise = g
  where
    h = hand (playerByName g p)

playRemodel g p
  | null h = g -- Nothing happens since there is nothing to trash
  | otherwise = queueSteps g [Left (PickACard p "Choose a card to trash: " h cont)]
  where
    h = hand (playerByName g p)
    cont card = [Right $ TrashCard p card,
                 Left (PickACard p "Choose a card to gain: "
                                 (filter ((<=maxCost) . cost) (availableCards g))
                                 (\c -> [Right $ GainCard (Discard p) c]))]
      where
        maxCost = 2 + cost card

playSpy g p = queueActions g [Draw p 1, GainActions 1, Branch lookAtTopCards]
  where
    lookAtTopCards game = queueSteps game { gen = gen', players = ps }
      $ map (Left . discardDecision)
      $ filter (\p -> canDraw p && not (moated p)) ps
      where
        (ps, gen') = St.runState (sequence $ map reshuffleIfNeeded (players game)) (gen game)
        reshuffleIfNeeded p = if null (deck p) && not (moated p) then reshuffleDiscard p else return p
        discardDecision player = YesNoDecision p ("Discard top of deck (" ++ show top ++ ") for player " ++ name player ++ "?")
                                              (\c -> [Right $ TransferCard top (TopOfDeck (name player)) (Discard (name player))])
          where
            top = head (deck player)

-- TODO implementation is messy and also imprecise because we ask trash/gain per card not for all at the end
playThief g thief = queueSteps g' decisions
  where
    g' = g { players = ps, gen = gen' }
    (ps, gen') = St.runState (sequence $ map reshuffleIfNeeded (players g)) (gen g)
    reshuffleIfNeeded p = if name p /= thief && length (deck p) < 2 then reshuffleDiscard p else return p
    decisions = opponents g' thief |> filter (null . deck) |> (\ops -> map reveal ops ++ concatMap mkDecision ops)
    reveal opponent = Left $ Information "All" (name opponent ++ " revealed " ++ show top2)
      where
        top2 = take 2 (deck opponent)
    mkDecision opponent
      | any isTreasure top2 =
        [Left $ PickACard thief "Pick a treasure to trash: " (filter isTreasure top2) cont]
      | otherwise = []
      where
        top2 = take 2 (deck opponent)
        cont c = [Left $ YesNoDecision thief ("Gain " ++ show c ++ " from trash?") (cont2 c)]
        cont2 c True = [Right $ TransferCard c (TopOfDeck (name opponent)) Trash,
                        Right $ TransferCard c Trash (Discard thief)]
                       ++ discardRest c
        cont2 c False = [Right $ TransferCard c (TopOfDeck (name opponent)) Trash] ++ discardRest c
        discardRest c = map (\c -> Right $ TransferCard c (TopOfDeck (name opponent)) (Discard (name opponent))) $ L.delete c top2


playThroneRoom g p = queueSteps g [Left (PickACard p "Choose an action: " (filter isAction (hand (playerByName g p))) cont)]
  where
    cont card = map Right [PlayCard card, PlayCopy card]

playCouncilRoom g player = queueActions g ([Draw player 4, GainBuys 1] ++ map (`Draw` 1) (opponentNames g player))

-- TODO potentially the "aside" part needs to be modelled explicitly
playLibrary g p = queueActions g [Branch repeatDraw]
  where
    condDraw s = if isAction newCard
                 then queueSteps s' [Left (YesNoDecision p ("Set aside action " ++ show newCard ++ "?") cont)]
                 else s'
      where
        s' = execute (Draw p 1) s
        newCard = head $ (hand (playerByName s' p) L.\\ hand (playerByName s p))
        cont True = [Right $ DiscardCard p newCard]
        cont False = []
    repeatDraw s
      | length (hand (activePlayer s)) < 7 && canDraw (activePlayer s) =
        queueActions s [Branch condDraw, Branch repeatDraw]
      | otherwise = s

playMine g p = if length treasuresInHand == 0
                then g
                else queueSteps g [Left (PickACard p "Choose a treasure to trash: "
                                                   treasuresInHand
                                                   cont)]
  where
    treasuresInHand = filter isTreasure (hand (playerByName g p))
    cont card = [Right (TrashCard p card),
                 Left (PickACard p "Choose a treasure card: "
                                 (filter (\c -> cost c <= cost card + 3 && isTreasure c) (availableCards g))
                                 cont2)]
    cont2 card = [Right (GainCard (Hand p) card)]

playWitch g player =
  queueActions g ([Draw player 2] ++ map (\p -> GainCard (Discard p) curse) (opponentNames g player))

playAdventurer g player = updatePlayerR g player inner
  where
    inner :: Player -> RandomState Player
    inner p = do (p', cards) <- drawIter p 2 []
                 return p' { discardPile = filter (not . isTreasure) cards ++ discardPile p',
                             hand = filter isTreasure cards ++ hand p' }

    drawIter :: Player -> Int -> [Card] -> RandomState (Player, [Card])
    drawIter p 0 cards = return (p, cards)
    drawIter p num cards =
      case p of
        Player { deck = [], discardPile = [] } -> return (p, cards)
        Player { deck = (x:xs) } -> drawIter (p {deck = xs}) (if isTreasure x then num-1 else num) (x:cards)
        _ -> reshuffleDiscard p >>= \p2 -> drawIter p2 num cards
