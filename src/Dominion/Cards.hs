module Dominion.Cards where

import Dominion.Model

import Data.Char (toLower)
import qualified Data.List as L
import qualified Data.Map.Strict as Map

kingdomCards :: [CardDef]
kingdomCards = concat [baseCards, intrigueCards, seasideCards, alchemyCards,
                       prosperityCards, cornucopiaCards, hinterlandCards,
                       darkAgesCards, guildsCards, adventuresCards,
                       promoCards]

cardData :: Map.Map String CardDef
cardData = Map.fromList $ map (\c -> (map toLower $ cardName c, c))
  (concat [basicCards, kingdomCards])

maybeCard :: String -> Maybe CardDef
maybeCard name = Map.lookup (map toLower name) cardData

lookupCard :: String -> CardDef
lookupCard name = cardData Map.! (map toLower name)

cSmithy = cardData Map.! "smithy"
cChancellor = cardData Map.! "chancellor"
cIsland = cardData Map.! "island"
cMoat = cardData Map.! "moat"

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
    checkAttack op state
      | null moats = attack op state
      | otherwise = decision (ChooseToReact (head moats) AttackTrigger (\b -> if b then toSimulation state else attack op state)) op state
      where
        moats = filter ((==cMoat) . typ) $ hand $ playerByName state op


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
    chooseToGain :: Card -> Action
    chooseToGain trashed player s2 =
      decision
        (ChooseCard (EffectGain unknownDef (Discard player))
                    (map (`topOfSupply` s2) (candidates trashed s2))
                    (\c -> gain (typ c) player s2))
        player s2
    candidates trashed state = affordableCards (addCost (cost state (typ trashed)) (simpleCost x)) state


enactEffect :: Effect -> Action
enactEffect (EffectPlusCards no) = plusCards no
enactEffect (EffectPlusActions no) = plusActions no
enactEffect (EffectPlusBuys no) = plusBuys no
enactEffect (EffectPlusMoney no) = plusMoney no
enactEffect (EffectTrashNo no) = trashNCards no no
enactEffect (EffectDiscardNo no) = discardNCards no
enactEffect (EffectGain def to) = gainTo def to
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
   actionA 109 "Feast" 4 feast,
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
  (chooseOne (EffectGain unknownDef (Discard player)) (map (`topOfSupply` state) (affordableCardsM 4 state))
   &&= \c -> gain (typ c))
  player state


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
        cont card = info AllPlayers (op ++ " reveals " ++ cardName (typ card))
          >> toSimulation (transfer card (Hand op) (TopOfDeck op) state)

feast :: Maybe Card -> Action
feast mCard player state =
  (case mCard of
    Just card -> trash card InPlay player state
    Nothing -> toSimulation state)
  `andThen` \s2 -> decision (ChooseCard (EffectGain unknownDef (Discard player))
                                        (map (`topOfSupply` s2) (affordableCardsM 5 s2))
                                        (\c -> gain (typ c) player s2))
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
  | null coppers = toSimulation state
  | otherwise = (trash (head coppers) (Hand player) &&& plusMoney 3) player state
  where
    coppers = filter ((==copper) . typ) $ hand $ playerByName state player

spy :: Action
spy player state = (plusCards 1 &&& plusActions 1 &&& spyAction &&& playAttack spyAction) player state
  where
    spyAction attackee state =
      do
        maybeS2 <- ensureCanDraw 1 state attackee
        maybe (toSimulation state) (doSpy attackee) maybeS2
    doSpy attackee state =
      info AllPlayers (attackee ++ " reveals " ++ cardName (typ top))
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
          `andThen` \s -> decision (ChooseToUse (EffectGainFrom card Trash (Hand player)) (\b -> if b then gainFrom card Trash player s else toSimulation s)) player s
          `andThen` \s2 -> seqSteps (\c -> discard c (TopOfDeck op) op) (L.delete card top) s2

throneRoom :: Action
throneRoom player state
  | null actions = toSimulation state
  | otherwise = decision (ChooseCard (EffectPlayCopy unknown) actions cont) player state
  where
    actions = filter isAction (hand (playerByName state player))
    cont card = (play card &&& playEffect (typ card) Nothing) player state

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
    gainDecision trashed s2 = decision (ChooseCard (EffectGain unknownDef (Hand player)) (affordable trashed s2)
                                          (\c -> gainTo (typ c) (Hand player) player s2))
                                       player s2
    affordable trashed state =
        filter isTreasure
        $ map (`topOfSupply` state)
        $ affordableCards (addCost (cost state (typ trashed)) (simpleCost 3)) state
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
   action 210 "Baron" 4 (plusBuys 1 &&& baron),
   action 211 "Bridge" 4 (plusBuys 1 &&& plusMoney 1 &&& addModifier (ModCost Nothing) (CappedDecModifier 1)),
   -- 212 conspirator
   -- 213 coppersmith
   action 214 "Ironworks" 4 ironworks,
   actionA 215 "Mining Village" 4 miningVillage,
   -- 216 scout
   carddef 217 "Duke" (simpleCost 5) [Victory] (\p -> length $ filter ((==duchy) . typ) (allCards p)) pass noTriggers,
   -- 218 minion
   -- 219 saboteur
   action 220 "Torturer" 5 (plusCards 3 &&& playAttack torturerAttack),
   action 221 "Trading Post" 5 tradingPost,
   -- 222 tribute
   action 223 "Upgrade" 5 (plusCards 1 &&& plusActions 1 &&& trashForGain upgradeBenefit),
   withInitialSupply (carddef 224 "Harem" (simpleCost 6) [Treasure, Victory] (const 2) (plusMoney 2) noTriggers)
    stdVictorySupply,
   withInitialSupply (carddef 225 "Nobles" (simpleCost 6) [Action, Victory] (const 2)
                      (chooseEffects 1 [EffectPlusCards 3, EffectPlusActions 2] enactEffects)
                      noTriggers)
                     stdVictorySupply
   ]

baron :: Action
baron player state
  | null estates =
    decision (ChooseToUse (EffectDiscard (head estates) (Hand player))
                             (\b -> if b
                                    then (discard (head estates) (Hand player) &&& plusMoney 4) player state
                                    else gain estate player state))
                player state
  | otherwise = gain estate player state
  where
    h = hand $ playerByName state player
    estates = filter ((==estate) . typ) h

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

ironworks player state =
  (chooseOne (EffectGain unknownDef (Discard player)) (map (`topOfSupply` state) (affordableCardsM 4 state))
   &&= \c -> gain (typ c) &&& ironBenefits c)
  player state
  where
    ironBenefits card =
      (if isAction card then plusActions 1 else pass)
      &&& (if isTreasure card then plusMoney 1 else pass)
      &&& (if isVictory card then plusCards 1 else pass)


miningVillage Nothing = plusCards 1 &&& plusActions 2
miningVillage (Just card) =
  plusCards 1
  &&& plusActions 2
  &&& (\player state -> optDecision (ChooseToUse (EffectTrash card InPlay) (cont player state)) player state)
  where
    cont _ state False = toSimulation state
    cont player state True = (trash card InPlay &&& plusMoney 2) player state

shantyDraw player state = (reveal h &&& if noActions then plusCards 2 else pass) player state
  where
    h = hand (playerByName state player)
    noActions = not $ any isAction h

torturerAttack op state = chooseEffects 1 [EffectDiscardNo 2, EffectGain curse (Hand op)] enactEffects op state

-- TODO what if a trigger replaces the trashing?
tradingPost player state
  | length h >= 2 = (trashNCards 2 2 &&& gainTo silver (Hand player)) player state
  | otherwise = trashNCards 2 2 player state
  where
    h = hand (playerByName state player)

upgradeBenefit trashed player state
  | null candidates = toSimulation state
  | otherwise = chooseOne (EffectGain unknownDef (Discard player)) candidates (gain . typ) player state
  where
    exactCost = addCost (cost state (typ trashed)) (simpleCost 1)
    candidates = map (`topOfSupply` state)
      $ filter ((==exactCost) . cost state)
      $ availableCards state

-- Seaside 3xx

seasideCards = map ($ Seaside)
  [-- 301 Embargo
   -- 302 Haven
   -- 303 Lighthouse
   -- 304 Native Village
   -- 305 Pearl Diver
   -- 306 Ambassador
   duration 307 "Fishing Village" 3 (plusActions 2 &&& plusMoney 1) (plusActions 1 &&& plusMoney 1),
   -- 308 Lookout
   -- 309 Smugglers
   action 310 "Warehouse" 3 (plusCards 3 &&& plusActions 1 &&& discardNCards 3),
   duration 311 "Caravan" 4 (plusCards 1 &&& plusActions 1) (plusCards 1),
   -- 312 Cutpurse
   withInitialSupply (carddefA 313 "Island" (simpleCost 4) [Action, Victory] (const 2) island noTriggers) stdVictorySupply,
   -- 314 Navigator
   -- 315 Pirate Ship
   -- 316 Salvager
   -- 317 Sea Hag
   -- 318 Treasure Map
   action 319 "Bazaar" 5 (plusCards 1 &&& plusActions 2 &&& plusMoney 1),
   -- 320 Explorer
   -- 321 Ghosh Ship
   duration 322 "Merchant Ship" 5 (plusMoney 2) (plusMoney 2),
   -- 323 Outpost
   -- 324 Tactician
   -- 325 Treasury
   duration 326 "Wharf" 5 (plusCards 2 &&& plusBuys 1) (plusCards 2 &&& plusBuys 1)
  ]

island mCard player state
  | null h = islandPut player state
  | otherwise = chooseOne (EffectPut unknown (Hand player) (Mat player IslandMat)) h cont player state
  where
    h = hand $ playerByName state player
    cont card = islandPut &&& put card (Hand player) (Mat player IslandMat)
    islandPut = case mCard of
      Just card -> put card InPlay (Mat player IslandMat)
      Nothing -> pass

-- Alchemy 4xx

alchemyCards = map ($ Alchemy)
  [-- 401 Herbalist
   action 402 "Apprentice" 5 (plusActions 1 &&& trashForGain apprenticeBenefit),
   -- 403 Transmute
   withInitialSupply (carddef 404 "Vineyard" (fullCost 0 1) [Victory] (\p -> length (filter isAction (allCards p)) `quot` 3) pass noTriggers)
    stdVictorySupply,
   -- 405 Apothecary -> needs ordering of cards
   -- 406 Scrying Pool
   carddef 407 "University" (fullCost 2 1) [Action] (const 0) (plusActions 2 &&& universityGain) noTriggers,
   -- 408 Alchemist
   carddef 409 "Familiar" (fullCost 3 1) [Action, Attack] (const 0) familiar noTriggers,
   carddef 410 "Philosopher's Stone" (fullCost 3 1) [Treasure] (const 0) philosophersStone noTriggers
   -- 411 Golem -> needs ordering of cards (can be simulated through repeated selection)
   -- 412 Possession
  ]

apprenticeBenefit card player state = (if drawNo == 0 then pass else plusCards drawNo) player state
  where
    drawNo = moneyCost c + 2 * potionCost c
    c = cost state (typ card)

familiar = plusCards 1 &&& plusActions 1 &&& playAttack (gain curse)
philosophersStone player state = plusMoney num player state
  where
    p = playerByName state player
    num = (length (discardPile p) + length (deck p)) `quot` 5

universityGain player state =
  optDecision (ChooseCard (EffectGain unknownDef (Discard player)) candidates (\c -> gain (typ c) player state)) player state
  where
    candidates = map (`topOfSupply` state) $ filter (`hasType` Action) $ affordableCardsM 5 state

-- Prosperity 5xx

prosperityCards = map ($ Prosperity)
  [-- 501 loan
   -- 502 trade route
   -- 503 watchtower
   -- 504 bishop
   action 505 "Monument" 4 (plusMoney 2 &&& plusTokens 1 VictoryToken),
   carddefA 506 "Quarry" (simpleCost 4) [Treasure] (const 0) quarryEffect noTriggers,
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
   withBuyRestriction
    (action 519 "Grand Market" 6 (plusCards 1 &&& plusActions 1 &&& plusBuys 1 &&& plusMoney 2))
    (\state -> not $ copper `elem` (map typ (inPlay (activePlayer state)))),
   -- 520 hoard
   -- 521 bank
   action 522 "Expand" 7 (remodelX 3),
   -- 523 forge
   action 524 "King's Court" 7 kingsCourt
   -- 525 peddler
   ]

quarryEffect Nothing = plusMoney 1
quarryEffect (Just card) =
  plusMoney 1 &&&
  addModifier (ModCost (Just Action)) (ConditionalModifier (\state -> card `elem` (inPlay $ activePlayer state)) (CappedDecModifier 2))

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
  | otherwise = optDecision (ChooseCard (EffectGain unknownDef (Discard player)) treasures (\card -> gain (typ card) player state)) player state
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
    cont card = (play card &&& playEffect (typ card) Nothing &&& playEffect (typ card) Nothing) player state


-- Cornucopia 6xx

cornucopiaCards = map ($ Cornucopia)
  [-- 601 hamlet
   -- 602 fortune teller
   action 603 "Menagerie" 3 (plusActions 1 &&& menagerie),
   -- 604 farming village
   -- 605 horse traders
   -- 606 remake
   -- 607 tournament
   -- 608 young witch
   -- 609 harvest
   -- 610 horn of plenty
   -- 611 hunting party
   -- 612 jester
   withInitialSupply (carddef 613 "Fairgrounds" (simpleCost 6) [Victory] fairgroundsPoints pass noTriggers)
    stdVictorySupply
  ]

fairgroundsPoints p = 2 * ((length $ L.nub $ map typ $ allCards p) `quot` 5)

menagerie player state = (reveal h &&& (if unique then plusCards 3 else plusCards 1)) player state
  where
    h = hand $ playerByName state player
    cardTypes = map typ h
    unique = length cardTypes == length (L.nub cardTypes)

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
   action 709 "Jack of All Trades" 4 jackOfAllTrades,
   -- 710 noble brigand
   -- 711 nomad camp
   withInitialSupply
    (carddef 712 "Silk Road" (simpleCost 4) [Victory] (\p -> length (filter isVictory (allCards p)) `quot` 4) pass noTriggers)
    stdVictorySupply
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
      info (VisibleToPlayer player) ("Top of deck is " ++ cardName (typ top))
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

darkAgesCards = map ($ DarkAges)
  [-- 801 Poor House
   -- 802 Beggar
   -- 803 Squire
   -- 804 Vagrant
   -- 805 Forager
   -- 806 Hermit
   -- 807 Market Square
   -- 808 Sage
   -- 809 Storeroom
   -- 810 Urchin
   action 811 "Armory" 4 armoryGain,
   -- 812 Death Cart
   withInitialSupply
     (carddef 813 "Feodum" (simpleCost 4) [Victory] (\p -> length (filter ((==silver) . typ) (allCards p)) `quot` 3)
        pass
        (Map.singleton TrashTrigger (gain silver &&& gain silver &&& gain silver)))
     stdVictorySupply,
   -- 814 Fortress
   -- 815 Ironmonger
   -- 816 Marauder
   -- 817 Procession
   -- 818 Rats
   -- 819 Scavenger
   -- 820 Wandering Minstrel
   -- 821 Band of Misfits
   -- 822 Bandit Camp
   -- 823 Catacombs
   -- 824 Count
   -- 825 Counterfeit
   -- 826 Cultist
   -- 827 Graverobber
   action 828 "Junk Dealer" 5 (plusCards 1 &&& plusActions 1 &&& plusMoney 1 &&& trashNCards 1 1)
   -- 829 Knights
   -- 830 Mystic
   -- 831 Pillage
   -- 832 Rebuild
   -- 833 Rogue
   -- 834 Altar
   -- 835 Hunting Grounds
  ]

armoryGain player state = decision
        (ChooseCard (EffectGain unknownDef (TopOfDeck player))
                    (map (`topOfSupply` state) candidates)
                    (\c -> gainTo (typ c) (TopOfDeck player) player state))
        player state
  where
    candidates = affordableCards (simpleCost 4) state

-- Guilds 9xx

guildsCards = []

-- Adventures 10xx

adventuresCards = []

-- Promo 20xx

promoCards = []