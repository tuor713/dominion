module Dominion.Cards where

import Dominion.Model

import Data.Char (toLower)
import qualified Data.List as L
import qualified Data.Map.Strict as Map

kingdomCards :: [Card]
kingdomCards = concat [baseCards, intrigueCards, seasideCards, alchemyCards,
                       prosperityCards, cornucopiaCards, hinterlandCards,
                       darkAgesCards, guildsCards, adventuresCards,
                       promoCards]

cardData :: Map.Map String Card
cardData = Map.fromList $ map (\c -> (map toLower $ cardName c, c))
  (concat [basicCards, kingdomCards])

maybeCard :: String -> Maybe Card
maybeCard name = Map.lookup (map toLower name) cardData

lookupCard :: String -> Card
lookupCard name = cardData Map.! (map toLower name)

cSmithy = cardData Map.! "smithy"
cChancellor = cardData Map.! "chancellor"
cIsland = cardData Map.! "island"

-- Generic action elements (potentially move to model)

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
        decision (ChooseToReact moat AttackTrigger (\b -> if b then toSimulation state else attack op state)) op state
      | otherwise = attack op state
      where
        h = hand $ playerByName state op


trashNCards :: Int -> Int -> Action
trashNCards inmin inmax player state =
  decision (ChooseCards (EffectTrash unknown (Hand player)) candidates (min',inmax) cont) player state
  where
    candidates = hand (playerByName state player)
    min' = min (length candidates) inmin
    cont cards = seqSteps (\c -> trash c (Hand player) player) cards state

discardNCards :: Int -> Action
discardNCards num player state = decision (ChooseCards (EffectDiscard unknown (Hand player)) candidates (num',num') cont) player state
  where
    candidates = hand (playerByName state player)
    num' = min (length candidates) num
    cont cards = seqSteps (\c -> discard c (Hand player) player) cards state

trashForGain :: (Card -> Action) -> Action
trashForGain gain player state
  | null h = toSimulation state
  | otherwise = decision (ChooseCard (EffectTrash unknown (Hand player)) h cont) player state
  where
    h = hand $ playerByName state player
    cont card = (trash card (Hand player) &&& gain card) player state

remodelX :: Int -> Action
remodelX x = trashForGain chooseToGain
  where
    chooseToGain trashed player s2 =
      decision
        (ChooseCard (EffectGain unknown Supply (Discard player))
                    (candidates trashed s2)
                    (\c -> gain c player s2))
        player s2
    candidates trashed state = affordableCards (addCost (cost (currentModifier state ModCost) trashed) (simpleCost x)) state


enactEffect :: Effect -> Action
enactEffect (EffectPlusCards no) = plusCards no
enactEffect (EffectPlusActions no) = plusActions no
enactEffect (EffectPlusBuys no) = plusBuys no
enactEffect (EffectPlusMoney no) = plusMoney no
enactEffect (EffectTrashNo no) = trashNCards no no
enactEffect (EffectDiscardNo no) = discardNCards no
enactEffect _ = error "Effect not implemented"

enactEffects :: [Effect] -> Action
enactEffects = foldr (\e a -> enactEffect e &&& a) pass



-- Base Edition

baseCards = map ($ Base)
  [action 101 "Cellar" 2 cellar,
   action 102 "Chapel" 2 chapel,
   carddef 103 "Moat" (simpleCost 2) [Action, Reaction] (const 0) (plusCards 2) noTriggers,

   action 104 "Chancellor" 3 chancellor,
   action 105 "Village" 3 (plusActions 2 &&& plusCards 1),
   action 106 "Woodcutter" 3 (plusBuys 1 &&& plusMoney 2),
   action 107 "Workshop" 3 workshop,

   attack 108 "Bureaucrat" 4 bureaucrat,
   action 109 "Feast" 4 feast,
   withInitialSupply (carddef 110 "Gardens" (simpleCost 4) [Victory] (\p -> length (allCards p) `quot` 10) pass noTriggers)
    stdVictorySupply,
   attack 111 "Militia" 4 militia,
   action 112 "Moneylender" 4 moneylender,
   action 113 "Remodel" 4 (remodelX 2),
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

cellar :: Action
cellar = plusActions 1
  &&+ \p s -> chooseMany (EffectDiscard unknown (Hand p)) (hand (playerByName s p)) (0,length $ (hand (playerByName s p)))
  &&= \cards -> seqActions (\c -> discard c (Hand p)) cards
  &&& plusCards (length cards)

chapel :: Action
chapel = trashNCards 0 4

chancellor :: Action
chancellor player state = plusMoney 2 player state
  `andThen` \s2 -> decision (ChooseToUse (SpecialEffect cChancellor) (cont s2)) player s2
  where
    cont state b = toSimulation $ if b then updatePlayer state player
                                                (\p -> p { deck = [], discardPile = deck p ++ discardPile p })
                                       else state

workshop :: Action
workshop player state =
  (chooseOne (EffectGain unknown Supply (Discard player)) (affordableCardsM 4 state) &&= gain) player state


bureaucrat :: Action
bureaucrat player state = (gainTo silver (TopOfDeck player) &&& playAttack treasureToDeck) player state
  where
    treasureToDeck op state
      | null treasures = info AllPlayers (op ++ " reveals hand " ++ summarizeCards h) >> toSimulation state
      -- TODO QDiscard is an approximation only
      | otherwise = decision (ChooseCard (EffectDiscard unknown (Hand op)) treasures cont) op state
      where
        h = hand $ playerByName state op
        treasures = filter isTreasure h
        cont card = info AllPlayers (op ++ " reveals " ++ cardName card)
          >> toSimulation (transfer card (Hand op) (TopOfDeck op) state)

feast :: Action
feast player state = trash (cardData Map.! "feast") InPlay player state
  `andThen` \s2 -> decision (ChooseCard (EffectGain unknown Supply (Discard player))
                                        (affordableCardsM 5 s2)
                                        (\c -> gain c player s2))
                            player s2

militia :: Action
militia player state = (plusMoney 2 &&& playAttack discardTo3) player state
  where
    discardTo3 op state
      | length h <= 3 = toSimulation state
      | otherwise = decision (ChooseCards (EffectDiscard unknown (Hand op)) h (length h - 3,length h - 3) cont) op state
      where
        h = hand $ playerByName state op
        cont cards = seqSteps (\c -> discard c (Hand op) op) cards state

moneylender :: Action
moneylender player state
  | copper `elem` h = (trash copper (Hand player) &&& plusMoney 3) player state
  | otherwise = toSimulation state
  where
    h = hand $ playerByName state player

spy :: Action
spy player state = (plusCards 1 &&& plusActions 1 &&& spyAction &&& playAttack spyAction) player state
  where
    spyAction attackee state =
      do
        maybeS2 <- ensureCanDraw 1 state attackee
        maybe (toSimulation state) (doSpy attackee) maybeS2
    doSpy attackee state =
      info AllPlayers (attackee ++ " reveals " ++ cardName top)
      >> decision (ChooseToUse (EffectDiscard top (TopOfDeck player)) cont) player state
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
      | otherwise = decision (ChooseCard (EffectTrash unknown (TopOfDeck op)) (filter isTreasure top) cont) player state
      where
        top = take 2 $ hand $ playerByName state op
        cont card = trash card (TopOfDeck op) op state
          `andThen` \s -> decision (ChooseToUse (EffectGain card Trash (Hand player)) (\b -> if b then gainFrom card Trash player s else toSimulation s)) player s
          `andThen` \s2 -> seqSteps (\c -> discard c (TopOfDeck op) op) (L.delete card top) s2

throneRoom :: Action
throneRoom player state
  | actions == [] = toSimulation state
  | otherwise = decision (ChooseCard (EffectPlayCopy unknown) actions cont) player state
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
      -- TODO check if this correctly handles aside
      | not (canDraw (activePlayer s2)) = toSimulation s2
      | isAction card = decision (ChooseToUse (EffectDiscard card (TopOfDeck name)) cont) name s2
      | otherwise = next
      where
        cont False = next
        cont True = library (card:aside) name (updatePlayer s2 name (\p -> p { deck = tail (deck p) }))
        next = (plusCards 1 &&& library aside) name s2
        card = head $ deck $ activePlayer s2

mine :: Action
mine player state
  | treasures == [] = toSimulation state
  | otherwise = decision (ChooseCard (EffectTrash unknown (Hand player)) treasures cont) player state
  where
    cont card = trash card (Hand player) player state `andThen` gainDecision card
    gainDecision trashed s2 = decision (ChooseCard (EffectGain unknown Supply (Hand player)) (affordable trashed s2) (\c -> gain c player s2)) player s2
    affordable trashed state =
      filter isTreasure $ affordableCards (addCost (cost (currentModifier state ModCost) trashed) (simpleCost 3)) state
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



-- Intrigue 2xx

intrigueCards = map ($ Intrigue)
  [action 201 "Courtyard" 2 courtyard,
   action 202 "Pawn" 2
          (chooseEffects 2 [EffectPlusCards 1, EffectPlusActions 1, EffectPlusBuys 1, EffectPlusMoney 1] enactEffects),
   -- 203 secret chamber
   withInitialSupply (carddef 204 "Great Hall" (simpleCost 3) [Action, Victory] (const 1) (plusCards 1 &&& plusActions 1) noTriggers)
    stdVictorySupply,
   -- 205 masquerade
   action 206 "Shanty Town" 3 (plusActions 2 &&& shantyDraw),
   action 207 "Steward" 3 (chooseEffects 1 [EffectPlusCards 2, EffectPlusMoney 2, EffectTrashNo 2] enactEffects),
   -- 208 swindler
   -- 209 wishing well
   -- 210 baron
   action 211 "Bridge" 4 (plusBuys 1 &&& plusMoney 1 &&& addModifier ModCost (CappedDecModifier 1)),
   -- 212 conspirator
   -- 213 coppersmith
   -- 214 ironworks
   -- 215 mining village
   -- 216 scout
   carddef 217 "Duke" (simpleCost 5) [Victory] (\p -> length $ filter (==duchy) (allCards p)) pass noTriggers,
   -- 218 minion
   -- 219 saboteur
   -- 220 torturer
   -- 221 trading post
   -- 222 tribute
   -- 223 upgrade
   withInitialSupply (carddef 224 "Harem" (simpleCost 6) [Treasure, Victory] (const 2) (plusMoney 2) noTriggers)
    stdVictorySupply,
   withInitialSupply (carddef 225 "Nobles" (simpleCost 6) [Action, Victory] (const 2)
                      (chooseEffects 1 [EffectPlusCards 3, EffectPlusActions 2] enactEffects)
                      noTriggers)
                     stdVictorySupply
   ]

courtyard = plusCards 3 &&& putOneBack
  where
    putOneBack :: Action
    putOneBack player state =
      chooseOne (EffectPut unknown (Hand player) (TopOfDeck player))
                (hand (playerByName state player))
                cont
                player
                state
    cont card player state = toSimulation $ transfer card (Hand player) (TopOfDeck player) state

shantyDraw player state = (reveal h &&& if noActions then plusCards 2 else pass) player state
  where
    h = hand (playerByName state player)
    noActions = not $ any isAction h

-- Seaside 3xx

seasideCards = map ($ Seaside)
  [-- 301 Embargo
   -- 302 Haven
   -- 303 Lighthouse
   -- 304 Native Village
   -- 305 Pearl Diver
   -- 306 Ambassador
   -- 307 Fishing Village
   -- 308 Lookout
   -- 309 Smugglers
   action 310 "Warehouse" 3 (plusCards 3 &&& plusActions 1 &&& discardNCards 3),
   -- 311 Caravan
   -- 312 Cutpurse
   withInitialSupply (carddef 313 "Island" (simpleCost 4) [Action, Victory] (const 2) island noTriggers) stdVictorySupply
   -- 314 Navigator
   -- 315 Pirate Ship
   -- 316 Salvager
   -- 317 Sea Hag
   -- 318 Treasure Map
   -- 319 Bazaar
   -- 320 Explorer
   -- 321 Ghosh Ship
   -- 322 Merchant Ship
   -- 323 Outpost
   -- 324 Tactician
   -- 325 Treasury
   -- 326 Wharf
  ]

island player state
  | null h = put cIsland InPlay (Mat player IslandMat) player state
  | otherwise = chooseOne (EffectPut unknown (Hand player) (Mat player IslandMat)) h cont player state
  where
    h = hand $ playerByName state player
    cont card = put cIsland InPlay (Mat player IslandMat) &&& put card (Hand player) (Mat player IslandMat)

-- Alchemy 4xx

alchemyCards = map ($ Alchemy)
  [-- 401 Herbalist
   -- 402 Apprentice
   -- 403 Transmute
   withInitialSupply (carddef 404 "Vineyard" (fullCost 0 1) [Victory] (\p -> length (filter isAction (allCards p)) `quot` 3) pass noTriggers)
    stdVictorySupply,
   -- 405 Apothecary -> needs ordering of cards
   -- 406 Scrying Pool
   -- 407 University
   -- 408 Alchemist
   carddef 409 "Familiar" (fullCost 3 1) [Action, Attack] (const 0) familiar noTriggers,
   carddef 410 "Philosopher's Stone" (fullCost 3 1) [Treasure] (const 0) philosophersStone noTriggers
   -- 411 Golem -> needs ordering of cards (can be simulated through repeated selection)
   -- 412 Possession
  ]

familiar = plusCards 1 &&& plusActions 1 &&& playAttack (gain curse)
philosophersStone player state = plusMoney num player state
  where
    p = playerByName state player
    num = (length (discardPile p) + length (deck p)) `quot` 5

-- Prosperity 5xx

prosperityCards = map ($ Prosperity)
  [-- 501 loan
   -- 502 trade route
   -- 503 watchtower
   -- 504 bishop
   -- 505 monument
   -- 506 quarry
   -- 507 talisman
   action 508 "Worker's Village" 4 (plusCards 1 &&& plusActions 2 &&& plusBuys 1),
   action 509 "City" 5 city,
   -- 510 contraband
   -- 511 counting house
   withTrigger (action 512 "Mint"5 mintAction) BuyTrigger mintTrigger,
   -- 513 mountebank
   -- 514 rabble
   -- 515 royal seal
   -- 516 vault
   -- 517 venture
   -- 518 goons
   -- 519 grand market
   -- 520 hoard
   -- 521 bank
   action 522 "Expand" 7 (remodelX 3),
   -- 523 forge
   action 524 "King's Court" 7 kingsCourt
   -- 525 peddler
   ]

city :: Action
city = plusCards 1
  &&& plusActions 2
  &&& \player state -> let num = length $ filter ((==0) . snd) $ supply state in extra num player state
  where
    extra 0 = pass
    extra 1 = plusCards 1
    extra _ = plusCards 1 &&& plusMoney 1 &&& plusBuys 1

mintAction :: Action
mintAction player state
  | null treasures = toSimulation state
  | otherwise = optDecision (ChooseCard (EffectGain unknown Supply (Discard player)) treasures (\card -> gain card player state)) player state
  where
    treasures = filter isTreasure $ hand (playerByName state player)

mintTrigger :: Action
mintTrigger player state = trashAll treasures InPlay player state
  where
    treasures = filter isTreasure $ inPlay (playerByName state player)

kingsCourt :: Action
kingsCourt player state
  | actions == [] = toSimulation state
  | otherwise = decision (ChooseCard (EffectPlayCopy unknown) actions cont) player state
  where
    actions = filter isAction (hand (playerByName state player))
    cont card = (play card &&& playEffect card &&& playEffect card) player state


-- Cornucopia 6xx

cornucopiaCards = []

-- Hinterlands 7xx

hinterlandCards = map ($ Hinterlands)
  [-- 701 crossroads
   -- 702 duchess
   -- 703 fool's gold
   -- 704 develop
   -- 705 oasis
   -- 706 oracle
   -- 707 scheme
   -- 708 tunnel
   action 709 "Jack of All Trades" 4 jackOfAllTrades
   -- 710 noble brigand
   -- 711 nomad camp
   -- 712 silk road
   -- 713 spice merchant
   -- 714 trader
   -- 715 cache
   -- 716 cartographer
   -- 717 embassy
   -- 718 haggler
   -- 719 highway
   -- 720 ill-gotten gains
   -- 721 inn
   -- 722 mandarin
   -- 723 margrave
   -- 724 stables
   -- 725 border village
   -- 726 farmland
   ]

jackOfAllTrades :: Action
jackOfAllTrades = gain silver &&& spyTop &&& drawTo5 &&& optTrash
  where
    spyTop player state = ensureCanDraw 1 state player >>= \maybeS2 -> maybe (toSimulation state) (chooseKeepDiscard player) maybeS2
    chooseKeepDiscard player state =
      info (VisibleToPlayer player) ("Top of deck is " ++ cardName top)
      >> decision (ChooseToUse (EffectDiscard top (TopOfDeck player)) cont) player state
      where
        top = head $ deck $ playerByName state player
        cont b = if b then discard top (TopOfDeck player) player state else toSimulation state

    drawTo5 player state = if num >= 5 then toSimulation state else plusCards (5-num) player state
      where
        num = length $ hand $ playerByName state player

    optTrash player state
      | null candidates = toSimulation state
      | otherwise = optDecision (ChooseCard (EffectTrash unknown (Hand player)) candidates cont) player state
      where
        candidates = filter (not . isTreasure) $ hand $ playerByName state player
        cont card = trash card (Hand player) player state


-- Dark Ages 8xx

darkAgesCards = []

-- Guilds 9xx

guildsCards = []

-- Adventures 10xx

adventuresCards = []

-- Promo 20xx

promoCards = []