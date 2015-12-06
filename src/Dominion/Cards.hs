module Dominion.Cards where

import Dominion.Model

import Data.Char (toLower)
import qualified Data.List as L
import qualified Data.Map.Strict as Map

cardData :: Map.Map String Card
cardData = Map.fromList $ map (\c -> (map toLower $ cardName c, c))
  (concat [basicCards, baseCards, intrigueCards, prosperityCards, hinterlandCards])

maybeCard :: String -> Maybe Card
maybeCard name = Map.lookup (map toLower name) cardData

lookupCard :: String -> Card
lookupCard name = cardData Map.! (map toLower name)


cSmithy = cardData Map.! "smithy"

-- Base Edition

baseCards = map ($ Base)
  [action 101 "Cellar" 2 cellar,
   action 102 "Chapel" 2 chapel,
   carddef 103 "Moat" 2 [Action, Reaction] (const 0) (plusCards 2),

   action 104 "Chancellor" 3 chancellor,
   action 105 "Village" 3 (plusActions 2 &&& plusCards 1),
   action 106 "Woodcutter" 3 (plusBuys 1 &&& plusMoney 2),
   action 107 "Workshop" 3 workshop,

   attack 108 "Bureaucrat" 4 bureaucrat,
   action 109 "Feast" 4 feast,
   carddef 110 "Gardens" 4 [Victory] (\p -> length (allCards p) `quot` 10) pass,
   attack 111 "Militia" 4 militia,
   action 112 "Moneylender" 4 moneylender,
   action 113 "Remodel" 4 remodel,
   action 114 "Smithy" 4 (plusCards 3),
   attack 115 "Spy" 4 spy,
   attack 116 "Thief" 4 thief,
   action 117 "Throne Room" 4 throneRoom,

   action 118 "Council Room" 5 councilRoom,
   action 119 "Festival" 5 (plusActions 2 &&& plusBuys 1 &&& plusMoney 2),
   action 120 "Laboratory" 5 (plusCards 2 &&& plusActions 1),
   action 121 "Library" 5 (library []),
   action 122 "Market" 5 (plusCards 1 &&& plusActions 1 &&& plusBuys 1 &&& plusMoney 1),
   action 123 "Mine" 5 mine,
   attack 124 "Witch" 5 witch,

   action 125 "Adventurer" 6 (adventurer [] [])
   ]

seqSteps :: (a -> GameState -> Simulation) -> [a] -> GameState -> Simulation
seqSteps _ [] state = toSimulation state
seqSteps f (x:xs) state = f x state `andThen` seqSteps f xs

seqActions :: (a -> Action) -> [a] -> Action
seqActions _ [] _ state = toSimulation state
seqActions f (x:xs) p state = f x p state `andThen` seqActions f xs p

playAttack :: Action -> Action
playAttack attack attacker state = seqSteps checkAttack (opponentNames state attacker) state
  where
    moat = (cardData Map.! "moat")
    -- TODO can we do this more elegantly, triggers again?
    checkAttack op state
      | moat `elem` h =
        decision (YesNo QOption moat (\b -> if b then toSimulation state else attack op state)) op state
      | otherwise = attack op state
      where
        h = hand $ playerByName state op

cellar :: Action
cellar = plusActions 1
  &&+ \p s -> chooseMany (QDiscard (Hand p)) (hand (playerByName s p)) (0,length $ (hand (playerByName s p)))
  &&= \cards -> seqActions (\c -> discard c (Hand p)) cards
  &&& plusCards (length cards)

chapel :: Action
chapel player state = decision (Choices QTrash (hand (playerByName state player)) (0,4) cont) player state
  where
    cont cards = seqSteps (\c -> trash c (Hand player) player) cards state

chancellor :: Action
chancellor player state = plusMoney 2 player state
  `andThen` \s2 -> decision (YesNo QOption (cardData Map.! "chancellor") (cont s2)) player s2
  where
    cont state b = toSimulation $ if b then updatePlayer state player
                                                (\p -> p { deck = [], discardPile = deck p ++ discardPile p })
                                       else state

workshop :: Action
workshop player state = (chooseOne QGain affordable &&= gain) player state
  where
    affordable = filter ((<=4) . cost) (availableCards state)

bureaucrat :: Action
bureaucrat player state = (gainTo silver (TopOfDeck player) &&& playAttack treasureToDeck) player state
  where
    treasureToDeck op state
      | null treasures = info AllPlayers (op ++ " reveals hand " ++ summarizeCards h) >> toSimulation state
      -- TODO QDiscard is an approximation only
      | otherwise = decision (Choice (QDiscard (Hand op)) treasures cont) op state
      where
        h = hand $ playerByName state op
        treasures = filter isTreasure h
        cont card = info AllPlayers (op ++ " reveals " ++ cardName card)
          >> toSimulation (transfer card (Hand op) (TopOfDeck op) state)

feast :: Action
feast player state = trash (cardData Map.! "feast") InPlay player state
  `andThen` \s2 -> decision (Choice QGain (affordable s2) (\c -> gain c player s2)) player s2
  where
    affordable state = filter ((<=5) . cost) (availableCards state)

militia :: Action
militia player state = (plusMoney 2 &&& playAttack discardTo3) player state
  where
    discardTo3 op state
      | length h <= 3 = toSimulation state
      | otherwise = decision (Choices (QDiscard (Hand op)) h (length h - 3,length h - 3) cont) op state
      where
        h = hand $ playerByName state op
        cont cards = seqSteps (\c -> discard c (Hand op) op) cards state

moneylender :: Action
moneylender player state
  | copper `elem` h = (trash copper (Hand player) &&& plusMoney 3) player state
  | otherwise = toSimulation state
  where
    h = hand $ playerByName state player

remodel :: Action
remodel player state
  | null h = toSimulation state
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
    spyAction attackee state =
      do
        maybeS2 <- ensureCanDraw 1 state attackee
        maybe (toSimulation state) (doSpy attackee) maybeS2
    doSpy attackee state =
      info AllPlayers (attackee ++ " reveals " ++ cardName top)
      >> decision (YesNo (QDiscard (TopOfDeck player)) top cont) player state
      where
        cont b = if b then discard top (TopOfDeck attackee) attackee state else toSimulation state
        top = head $ deck $ playerByName state attackee

thief :: Action
thief player state = playAttack doThief player state
  where
    doThief :: Action
    doThief op state =
      do
        maybeS2 <- ensureCanDraw 2 state op
        maybeS3 <- case maybeS2 of
                    (Just _) -> return maybeS2
                    Nothing -> ensureCanDraw 1 state op
        maybe (toSimulation state) (decide op) maybeS3
    decide op state
      | null (filter isTreasure top) = seqSteps (\c -> discard c (TopOfDeck op) op) top state
      | otherwise = decision (Choice QTrash (filter isTreasure top) cont) player state
      where
        top = take 2 $ hand $ playerByName state op
        cont card = trash card (TopOfDeck op) op state
          `andThen` \s -> decision (YesNo QGain card (\b -> if b then gainFrom card Trash player s else toSimulation s)) player s
          `andThen` \s2 -> seqSteps (\c -> discard c (TopOfDeck op) op) (L.delete card top) s2

throneRoom :: Action
throneRoom player state
  | actions == [] = toSimulation state
  | otherwise = decision (Choice QPlay actions cont) player state
  where
    actions = filter isAction (hand (playerByName state player))
    cont card = (play card &&& playEffect card) player state

councilRoom :: Action
councilRoom player state = (plusCards 4 &&& plusBuys 1) player state `andThen` seqSteps (plusCards 1) (opponentNames state player)

library :: [Card] -> Action
library aside name state
  | length (hand (playerByName state name)) >= 7 = toSimulation finalState
  | otherwise = ensureCanDraw 1 state name >>= \maybeS2 -> maybe (toSimulation finalState) draw1 maybeS2
  where
    finalState = updatePlayer state name (\p -> p { discardPile = aside ++ discardPile p})
    draw1 s2
      | not (canDraw (activePlayer s2)) = toSimulation s2
      | isAction card = decision (YesNo QDraw card cont) name s2
      | otherwise = next
      where
        cont True = next
        cont False = library (card:aside) name (updatePlayer s2 name (\p -> p { deck = tail (deck p) }))
        next = (plusCards 1 &&& library aside) name s2
        card = head $ deck $ activePlayer s2

mine :: Action
mine player state
  | treasures == [] = toSimulation state
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
  | otherwise =  ensureCanDraw 1 state player >>= \maybeS2 -> maybe terminal draw1 maybeS2
  where
    -- TODO this should probably trigger discard
    terminal = toSimulation (updatePlayer state player (\p -> p { hand = treasures ++ hand p, discardPile = others ++ discardPile p }))
    draw1 state = adventurer (if isTreasure top then top:treasures else treasures)
                             (if isTreasure top then others else top:others)
                             player s2
      where
        top = head $ hand (playerByName state player)
        s2 = updatePlayer state player (\p -> p { deck = tail (deck p) })



-- Intrigue

intrigueCards = map ($ Intrigue)
  [carddef 117 "Duke" 5 [Victory] (\p -> length $ filter (==duchy) (allCards p)) pass]


-- Prosperity

prosperityCards = map ($ Prosperity)
  [-- 401 loan
   -- 402 trade route
   -- 403 watchtower
   -- 404 bishop
   -- 405 monument
   -- 406 quarry
   -- 407 talisman
   action 408 "Worker's Village" 4 (plusCards 1 &&& plusActions 2 &&& plusBuys 1),
   action 409 "City" 5 city,
   -- 410 contraband
   -- 411 counting house
   -- 412 mint
   -- 413 mountebank
   -- 414 rabble
   -- 415 royal seal
   -- 416 vault
   -- 417 venture
   -- 418 goons
   -- 419 grand market
   -- 420 hoard
   -- 421 bank
   -- 422 expand
   -- 423 forge
   action 424 "King's Court" 7 kingsCourt
   -- 425 peddler
   ]

city :: Action
city = plusCards 1
  &&& plusActions 2
  &&& \player state -> let num = length $ filter ((==0) . snd) $ supply state in extra num player state
  where
    extra 0 = pass
    extra 1 = plusCards 1
    extra _ = plusCards 1 &&& plusMoney 1 &&& plusBuys 1

kingsCourt :: Action
kingsCourt player state
  | actions == [] = toSimulation state
  | otherwise = decision (Choice QPlay actions cont) player state
  where
    actions = filter isAction (hand (playerByName state player))
    cont card = (play card &&& playEffect card &&& playEffect card) player state


-- Hinterlands

hinterlandCards = map ($ Hinterlands)
  [-- 601 crossroads
   -- 602 duchess
   -- 603 fool's gold
   -- 604 develop
   -- 605 oasis
   -- 606 oracle
   -- 607 scheme
   -- 608 tunnel
   action 609 "Jack of All Trades" 4 jackOfAllTrades
   -- 610 noble brigand
   -- 611 nomad camp
   -- 612 silk road
   -- 613 spice merchant
   -- 614 trader
   -- 615 cache
   -- 616 cartographer
   -- 617 embassy
   -- 618 haggler
   -- 619 highway
   -- 620 ill-gotten gains
   -- 621 inn
   -- 622 mandarin
   -- 623 margrave
   -- 624 stables
   -- 625 border village
   -- 626 farmland
   ]

jackOfAllTrades :: Action
jackOfAllTrades = gain silver &&& spyTop &&& drawTo5 &&& optTrash
  where
    spyTop player state = ensureCanDraw 1 state player >>= \maybeS2 -> maybe (toSimulation state) (chooseKeepDiscard player) maybeS2
    chooseKeepDiscard player state =
      info (VisibleToPlayer player) ("Top of deck is " ++ cardName top)
      >> decision (YesNo (QDiscard (TopOfDeck player)) top cont) player state
      where
        top = head $ deck $ playerByName state player
        cont b = if b then discard top (TopOfDeck player) player state else toSimulation state

    drawTo5 player state = if num >= 5 then toSimulation state else plusCards (5-num) player state
      where
        num = length $ hand $ playerByName state player

    optTrash player state
      | null candidates = toSimulation state
      | otherwise = optDecision (Choice QTrash candidates cont) player state
      where
        candidates = filter (not . isTreasure) $ hand $ playerByName state player
        cont card = trash card (Hand player) player state
