module Dominion.Cards where

import Dominion.Model

import qualified Control.Monad as M
import qualified Control.Monad.Trans.State.Lazy as St
import Data.Char (toLower)
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import System.Random (StdGen, mkStdGen, randomR, newStdGen)


noPoints = const 0

treasure name cost money edition = Card name edition cost [Treasure] noPoints (plusMoney money)
action name cost effect edition = Card name edition cost [Action] noPoints effect
attack name cost effect edition = Card name edition cost [Action, Attack] noPoints effect
victory name cost points edition = Card name edition cost [Victory] (const points) pass
carddef name cost types points effect edition = Card name edition cost types points effect


cardData :: Map.Map String Card
cardData = Map.fromList $ map (\c -> (map toLower $ cardName c, c))
  (concat [baseCards, prosperityCards, hinterlandCards])

maybeCard :: String -> Maybe Card
maybeCard name = Map.lookup (map toLower name) cardData

lookupCard :: String -> Card
lookupCard name = cardData Map.! (map toLower name)


-- Dummy card for hidden information
unknown = Card "Unknown" Base 0 [] noPoints pass
curse = cardData Map.! "curse"

estate = cardData Map.! "estate"
duchy = cardData Map.! "duchy"
province = cardData Map.! "province"
colony = cardData Map.! "colony"

copper = cardData Map.! "copper"
silver = cardData Map.! "silver"
gold = cardData Map.! "gold"
platinum = cardData Map.! "platinum"


-- Common patterns





-- Base Edition

baseCards = map ($ Base)
  [treasure "Copper" 0 1,
   treasure "Silver" 3 2,
   treasure "Gold" 6 3,

   victory "Estate" 2 1,
   victory "Duchy" 5 3,
   victory "Province" 8 6,

   carddef "Curse" 0 [CurseType] (const (-1)) pass,

   action "Cellar" 2 cellar,
   action "Chapel" 2 chapel,
   carddef "Moat" 2 [Action, Reaction] (const 0) (plusCards 2),

   action "Chancellor" 3 chancellor,
   action "Village" 3 (plusActions 2 &&& plusCards 1),
   action "Woodcutter" 3 (plusBuys 1 &&& plusMoney 2),
   action "Workshop" 3 workshop,

   attack "Bureaucrat" 4 bureaucrat,
   action "Feast" 4 feast,
   carddef "Gardens" 4 [Victory] (\p -> length (allCards p) `quot` 10) pass,
   attack "Militia" 4 militia,
   action "Moneylender" 4 moneylender,
   action "Remodel" 4 remodel,
   action "Smithy" 4 (plusCards 3),
   attack "Spy" 4 spy,
   attack "Thief" 4 thief,
   action "Throne Room" 4 throneRoom,

   action "Council Room" 5 councilRoom,
   action "Festival" 5 (plusActions 2 &&& plusBuys 1 &&& plusMoney 2),
   action "Laboratory" 5 (plusCards 2 &&& plusActions 1),
   action "Library" 5 (library []),
   action "Market" 5 (plusCards 1 &&& plusActions 1 &&& plusBuys 1 &&& plusMoney 1),
   action "Mine" 5 mine,
   attack "Witch" 5 witch,

   action "Adventurer" 6 (adventurer [] [])
   ]

seqSteps :: (a -> GameState -> GameStep) -> [a] -> GameState -> GameStep
seqSteps f [] state = State state
seqSteps f (x:xs) state = f x state `andThen` seqSteps f xs

seqActions :: (a -> Action) -> [a] -> Action
seqActions _ [] _ state = State state
seqACtions f (x:xs) p state = f x p state `andThen` seqActions f xs p

playAttack :: Action -> Action
playAttack attack attacker state = seqSteps checkAttack (opponentNames state attacker) state
  where
    moat = (cardData Map.! "moat")
    -- TODO can we do this more elegantly, triggers again?
    checkAttack op state
      | moat `elem` h =
        decision (YesNo (QOption moat) (\b -> if b then State state else attack op state)) op state
      | otherwise = attack op state
      where
        h = hand $ playerByName state op

cellar :: Action
cellar = plusActions 1
  &&+ \p s -> chooseMany QDiscard (hand (playerByName s p)) (const True)
  &&= \cards -> seqActions (\c -> discard c (Hand p)) cards
  &&& plusCards (length cards)

chapel :: Action
chapel player state = decision (Choices QTrash (hand (playerByName state player)) valid cont) player state
  where
    valid cards = length cards <= 4
    cont cards = seqSteps (\c -> trash c (Hand player) player) cards state

chancellor :: Action
chancellor player state = plusMoney 2 player state
  `andThen` \s2 -> decision (YesNo (QOption (cardData Map.! "chancellor")) (cont s2)) player s2
  where
    cont state b = State $ if b then updatePlayer state player
                                       (\p -> p { deck = [], discardPile = deck p ++ discardPile p })
                                else state

workshop :: Action
workshop player state = app player state $ chooseOne QGain affordable &&= gain
  where
    affordable = filter ((<=4) . cost) (availableCards state)

bureaucrat :: Action
bureaucrat player state = (gainTo silver (TopOfDeck player) &&& playAttack treasureToDeck) player state
  where
    treasureToDeck op state
      | null treasures = info (op ++ " reveals hand " ++ summarizeCards h) allPlayerId state
      -- TODO QDiscard is an approximation only
      | otherwise = decision (Choice QDiscard treasures cont) op state
      where
        h = hand $ playerByName state op
        treasures = filter isTreasure h
        cont card = info (op ++ " reveals " ++ cardName card) allPlayerId state
          `andThen` (State . transfer card (Hand op) (TopOfDeck op))

feast :: Action
feast player state = trash (cardData Map.! "feast") InPlay player state
  `andThen` \s2 -> decision (Choice QGain (affordable s2) (\c -> gain c player s2)) player s2
  where
    affordable state = filter ((<=5) . cost) (availableCards state)

militia :: Action
militia player state = (plusMoney 2 &&& playAttack discardTo3) player state
  where
    discardTo3 op state
      | length h <= 3 = State state
      | otherwise = decision (Choices QDiscard h (\cs -> length h == length cs + 3) cont) op state
      where
        h = hand $ playerByName state op
        cont cards = seqSteps (\c -> discard c (Hand op) op) cards state

moneylender :: Action
moneylender player state
  | copper `elem` h = (trash copper (Hand player) &&& plusMoney 3) player state
  | otherwise = State state
  where
    h = hand $ playerByName state player

remodel :: Action
remodel player state
  | null h = State state
  | otherwise = decision (Choice QTrash h cont) player state
  where
    h = hand $ playerByName state player
    cont card = trash card (Hand player) player state `andThen` chooseToGain card
    chooseToGain trashed s2 = decision (Choice QGain (affordable trashed s2) (cont2 s2)) player s2
    affordable trashed state = filter ((<= cost trashed + 2) . cost) (availableCards state)
    cont2 state card = gain card player state

spy :: Action
spy player state = (plusCards 1 &&& plusActions 1 &&& spyAction &&& playAttack spyAction) player state
  where
    spyAction attackee state = maybe (State state) (doSpy attackee) $ ensureCanDraw 1 state attackee
    doSpy attackee state =
      info (attackee ++ " reveals " ++ cardName top) allPlayerId state
      `andThen` decision (YesNo QDiscard cont) player
      where
        cont b = if b then discard top (TopOfDeck attackee) attackee state else State state
        top = head $ deck $ playerByName state attackee

thief :: Action
thief player state = playAttack doThief player state
  where
    doThief op state =
      maybe (State state) (decide op) (M.mplus (ensureCanDraw 2 state op) (ensureCanDraw 1 state op))
    decide op state
      | null (filter isTreasure top) = seqSteps (\c -> discard c (TopOfDeck op) op) top state
      | otherwise = decision (Choice QTrash (filter isTreasure top) cont) player state
      where
        top = take 2 $ hand $ playerByName state op
        cont card = trash card (TopOfDeck op) op state
          `andThen` \s -> decision (YesNo QGain (\b -> if b then gainFrom card Trash player s else State s)) player s
          `andThen` \s2 -> seqSteps (\c -> discard c (TopOfDeck op) op) (L.delete card top) s2

throneRoom :: Action
throneRoom player state
  | actions == [] = State state
  | otherwise = decision (Choice QPlay actions cont) player state
  where
    actions = filter isAction (hand (playerByName state player))
    cont card = (play card &&& playEffect card) player state

councilRoom :: Action
councilRoom player state = (plusCards 4 &&& plusBuys 1) player state `andThen` seqSteps (plusCards 1) (opponentNames state player)

library :: [Card] -> Action
library aside name state
  | length (hand (playerByName state name)) >= 7 = State finalState
  | otherwise = maybe (State finalState) draw1 (ensureCanDraw 1 state name)
  where
    finalState = updatePlayer state name (\p -> p { discardPile = aside ++ discardPile p})
    draw1 s2
      | not (canDraw (activePlayer s2)) = State s2
      | isAction card = decision (YesNo QDraw cont) name s2
      | otherwise = next
      where
        cont True = next
        cont False = library (card:aside) name (updatePlayer s2 name (\p -> p { deck = tail (deck p) }))
        next = (plusCards 1 &&& library aside) name s2
        card = head $ deck $ activePlayer s2

mine :: Action
mine player state
  | treasures == [] = State state
  | otherwise = decision (Choice QTrash treasures cont) player state
  where
    cont card = trash card (Hand player) player state `andThen` gainDecision card
    gainDecision trashed s2 = decision (Choice QGain (affordable trashed s2) (\c -> gain c player s2)) player s2
    affordable trashed state = filter (\c -> cost c <= cost trashed + 3 && isTreasure c) (availableCards state)
    treasures = filter isTreasure (hand (playerByName state player))

witch :: Action
witch = plusCards 2 &&& playAttack (gain curse)

-- TODO all home grown primitives rather than reusable pieces
adventurer :: [Card] -> [Card] -> Action
adventurer treasures others player state
  | length treasures >= 2 = terminal
  | otherwise = maybe terminal draw1 $ ensureCanDraw 1 state player
  where
    -- TODO this should probably trigger discard
    terminal = State (updatePlayer state player (\p -> p { hand = treasures ++ hand p, discardPile = others ++ discardPile p }))
    draw1 state = adventurer (if isTreasure top then top:treasures else treasures)
                             (if isTreasure top then others else top:others)
                             player s2
      where
        top = head $ hand (playerByName state player)
        s2 = updatePlayer state player (\p -> p { deck = tail (deck p) })



-- Prosperity

prosperityCards = map ($ Prosperity)
  [treasure "Platinum" 9 5,
   victory "Colony" 11 10]


-- Hinterlands

hinterlandCards = map ($ Hinterlands)
  [action "Jack of All Trades" 4 jackOfAllTrades]

jackOfAllTrades :: Action
jackOfAllTrades = gain silver &&& spyTop &&& drawTo5 &&& optTrash
  where
    spyTop player state = maybe (State state) (chooseKeepDiscard player) (ensureCanDraw 1 state player)
    chooseKeepDiscard player state = (info ("Top of deck is " ++ cardName top) &&& decision (YesNo QDiscard cont)) player state
      where
        top = head $ deck $ playerByName state player
        cont b = if b then discard top (TopOfDeck player) player state else State state

    drawTo5 player state = if num >= 5 then State state else plusCards (5-num) player state
      where
        num = length $ hand $ playerByName state player

    optTrash player state
      | null candidates = State state
      | otherwise = optDecision (Choice QTrash candidates cont) player state
      where
        candidates = filter (not . isTreasure) $ hand $ playerByName state player
        cont card = trash card (Hand player) player state
