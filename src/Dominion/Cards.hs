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

cSmithy        = cardData Map.! "smithy"
cChancellor    = cardData Map.! "chancellor"
cIsland        = cardData Map.! "island"
cMoat          = cardData Map.! "moat"
cPort          = cardData Map.! "port"
cBorderVillage = cardData Map.! "border village"
cMinion        = cardData Map.! "minion"
cMint          = cardData Map.! "mint"
cCache         = cardData Map.! "cache"

-- Generic action elements (potentially move to model)

eachOtherPlayer :: Action -> Action
eachOtherPlayer action player state = seqSteps action (opponentNames state player) state

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

gainUpto :: Int -> Action
gainUpto x player state
  | null candidates = toSimulation state
  | otherwise =
    decision
      (ChooseCard (EffectGain unknownDef (Discard player))
        (map (`topOfSupply` state) candidates)
        (\c -> gain (typ c) player state))
      player state
  where
    candidates = (affordableCards (simpleCost x) state)


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
enactEffect (EffectPut card from to) = put card from to
enactEffect (EffectTrash card from) = trash card from
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
   notImplemented "Secret Chamber",
   withInitialSupply (carddef 204 "Great Hall" (simpleCost 3) [Action, Victory] (const 1) (plusCards 1 &&& plusActions 1) noTriggers)
    stdVictorySupply,
   action 205 "Masquerade" 3 (plusCards 2 &&& masquerade),
   action 206 "Shanty Town" 3 (plusActions 2 &&& shantyDraw),
   action 207 "Steward" 3 (chooseEffects 1 [EffectPlusCards 2, EffectPlusMoney 2, EffectTrashNo 2] enactEffects),
   notImplemented "Swindler", -- 208
   notImplemented "Wishing Well", -- 209
   action 210 "Baron" 4 (plusBuys 1 &&& baron),
   action 211 "Bridge" 4 (plusBuys 1 &&& plusMoney 1 &&& addModifier (ModCost Nothing) (CappedDecModifier 1)),
   notImplemented "Conspirator",-- 212
   notImplemented "Coppersmith", -- 213
   action 214 "Ironworks" 4 ironworks,
   actionA 215 "Mining Village" 4 miningVillage,
   notImplemented "Scout", -- 216
   carddef 217 "Duke" (simpleCost 5) [Victory] (\p -> length $ filter ((==duchy) . typ) (allCards p)) pass noTriggers,
   attack 218 "Minion" 5 (plusActions 1 &&& minionAttack),
   notImplemented "Saboteur", -- 219 saboteur
   action 220 "Torturer" 5 (plusCards 3 &&& playAttack torturerAttack),
   action 221 "Trading Post" 5 tradingPost,
   notImplemented "Tribute", -- 222 tribute
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
  | not (null estates) =
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

masquerade player state = (iter (turnOrder state ++ [head $ turnOrder state]) &&& trashNCards 0 1) player state
  where
    iter :: [PlayerId] -> Action
    iter (a:b:xs) player state =
      decision (ChooseCard (EffectPut unknown (Hand a) (Hand b))
                           (hand $ playerByName state a)
                           (\card -> (iter (b:xs) &&& put card (Hand a) (Hand b)) player state))
               a state
    iter _ _ state = toSimulation state



miningVillage Nothing = plusCards 1 &&& plusActions 2
miningVillage (Just card) =
  plusCards 1
  &&& plusActions 2
  &&& (\player state -> optDecision (ChooseToUse (EffectTrash card InPlay) (cont player state)) player state)
  where
    cont _ state False = toSimulation state
    cont player state True = (trash card InPlay &&& plusMoney 2) player state

minionAttack = chooseEffects 1 [EffectPlusMoney 2, SpecialEffect cMinion] act
  where
    act [SpecialEffect _] player state =
      ((foldr (&&&) pass (map (\c -> discard c (Hand player)) (hand $ playerByName state player)))
       &&& plusCards 4
       &&& playAttack attack)
        player
        state
    act [a] player state = enactEffect a player state
    act _ _ _ = error "Minion attack got back other than singleton effect"

    attack op state =
      if length (hand $ playerByName state op) > 4
      then (foldr (&&&) (plusCards 4) (map (\c -> discard c (Hand op)) (hand $ playerByName state op)))
            op state
      else toSimulation state

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
  [notImplemented "Embargo", -- 301 Embargo
   notImplemented "Haven", -- 302 Haven
   notImplemented "Lighthouse", -- 303 Lighthouse
   notImplemented "Native Village", -- 304 Native Village
   notImplemented "Pearl Diver", -- 305 Pearl Diver
   notImplemented "Ambassador", -- 306 Ambassador
   duration 307 "Fishing Village" 3 (plusActions 2 &&& plusMoney 1) (plusActions 1 &&& plusMoney 1),
   notImplemented "Lookout", -- 308 Lookout
   notImplemented "Smugglers", -- 309 Smugglers
   action 310 "Warehouse" 3 (plusCards 3 &&& plusActions 1 &&& discardNCards 3),
   duration 311 "Caravan" 4 (plusCards 1 &&& plusActions 1) (plusCards 1),
   notImplemented "Cutpurse", -- 312 Cutpurse
   withInitialSupply (carddefA 313 "Island" (simpleCost 4) [Action, Victory] (const 2) island noTriggers) stdVictorySupply,
   notImplemented "Navigator", -- 314 Navigator
   notImplemented "Pirate Ship", -- 315 Pirate Ship
   action 316 "Salvager" 4 (plusBuys 1 &&& trashForGain salvager),
   notImplemented "Sea Hag", -- 317 Sea Hag
   notImplemented "Treasure Map", -- 318 Treasure Map
   action 319 "Bazaar" 5 (plusCards 1 &&& plusActions 2 &&& plusMoney 1),
   notImplemented "Explorer", -- 320 Explorer
   notImplemented "Ghost Ship", -- 321 Ghosh Ship
   duration 322 "Merchant Ship" 5 (plusMoney 2) (plusMoney 2),
   notImplemented "Outpost", -- 323 Outpost
   notImplemented "Tactician", -- 324 Tactician
   notImplemented "Treasury", -- 325 Treasury
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

salvager card player state = plusMoney (moneyCost (cost state (typ card))) player state

-- Alchemy 4xx

alchemyCards = map ($ Alchemy)
  [notImplemented "Herbalist", -- 401 Herbalist
   action 402 "Apprentice" 5 (plusActions 1 &&& trashForGain apprenticeBenefit),
   notImplemented "Transmute", -- 403 Transmute
   withInitialSupply (carddef 404 "Vineyard" (fullCost 0 1) [Victory] (\p -> length (filter isAction (allCards p)) `quot` 3) pass noTriggers)
    stdVictorySupply,
   notImplemented "Apothecary", -- 405 Apothecary -> needs ordering of cards
   notImplemented "Scrying Pool", -- 406 Scrying Pool
   carddef 407 "University" (fullCost 2 1) [Action] (const 0) (plusActions 2 &&& universityGain) noTriggers,
   notImplemented "Alchemist", -- 408 Alchemist
   carddef 409 "Familiar" (fullCost 3 1) [Action, Attack] (const 0) familiar noTriggers,
   carddef 410 "Philosopher's Stone" (fullCost 3 1) [Treasure] (const 0) philosophersStone noTriggers,
   notImplemented "Golem", -- 411 Golem -> needs ordering of cards (can be simulated through repeated selection)
   notImplemented "Possession" -- 412 Possession
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
  [notImplemented "Loan", -- 501 loan
   notImplemented "Trade Route", -- 502 trade route
   carddef 503 "Watchtower" (simpleCost 3) [Action, Reaction] noPoints watchtower (whileInHand watchtowerTrigger),
   action 504 "Bishop" 4 (plusMoney 1 &&& plusTokens 1 VictoryToken &&& bishop),
   action 505 "Monument" 4 (plusMoney 2 &&& plusTokens 1 VictoryToken),
   carddefA 506 "Quarry" (simpleCost 4) [Treasure] (const 0) quarryEffect noTriggers,
   withTrigger (treasure 507 "Talisman" 4 1) (whileInPlay (onBuy talismanTrigger)),
   action 508 "Worker's Village" 4 (plusCards 1 &&& plusActions 2 &&& plusBuys 1),
   action 509 "City" 5 city,
   notImplemented "Contraband", -- 510 contraband
   action 511 "Counting House" 5 countingHouse,
   withTrigger (action 512 "Mint" 5 mintAction) (onBuySelf mintTrigger),
   notImplemented "Mountebank", -- 513 mountebank
   notImplemented "Rabble", -- 514 rabble
   withTrigger (treasure 515 "Royal Seal" 5 2) (whileInPlay royalSealTrigger),
   action 516 "Vault" 5 (plusCards 2 &&& vault),
   notImplemented "Venture", -- 517 venture
   notImplemented "Goons", -- 518 goons
   withBuyRestriction
    (action 519 "Grand Market" 6 (plusCards 1 &&& plusActions 1 &&& plusBuys 1 &&& plusMoney 2))
    (\state -> not $ copper `elem` (map typ (inPlay (activePlayer state)))),
   withTrigger (treasure 520 "Hoard" 6 2) (whileInPlay (onBuy (\def -> if hasType def Victory then gain gold else pass))),
   carddef 521 "Bank" (simpleCost 7) [Treasure] noPoints bank noTriggers,
   action 522 "Expand" 7 (remodelX 3),
   notImplemented "Forge", -- 523 forge
   action 524 "King's Court" 7 kingsCourt,
   notImplemented "Peddler" -- 525 peddler
   ]

watchtower player state
  | length h >= 6 = toSimulation state
  | otherwise = plusCards (6 - length h) player state
  where
    h = hand $ playerByName state player

watchtowerReplay :: CardDef -> Location -> Action
watchtowerReplay def loc player state
  | null cands = toSimulation state
  | otherwise = chooseEffects 1 [EffectTrash (head cands) loc, EffectPut (head cands) loc (TopOfDeck player)] enactEffects player state
  where
    cands = filter ((==def) . typ) $ getCards loc state

watchtowerTrigger :: TriggerHandler
watchtowerTrigger GainTrigger (EffectGain def to) (Left w) cont =
  cont &&& reveal [w] &&& \player state -> decision (ChooseToReact w GainTrigger (\b -> if b then watchtowerReplay def to player state else toSimulation state)) player state
watchtowerTrigger GainTrigger (EffectGainFrom card _ to) (Left w) cont =
  cont &&& reveal [w] &&& \player state -> decision (ChooseToReact w GainTrigger (\b -> if b then watchtowerReplay (typ card) to player state else toSimulation state)) player state
watchtowerTrigger _ _ _ cont = cont

royalSealTrigger :: TriggerHandler
royalSealTrigger GainTrigger (EffectGain def to) _ cont =
  cont &&& \player state ->
    let cands = filter ((==def) . typ) (getCards to state) in
    if null cands
    then toSimulation state
    else optDecision (ChooseToUse (EffectPut (head cands) (Discard player) (TopOfDeck player))
                                  (\b -> if b then put (head cands) (Discard player) (TopOfDeck player) player state
                                              else toSimulation state))
          player state
royalSealTrigger GainTrigger (EffectGainFrom card _ to) _ cont =
  cont &&& \player state ->
    let cands = filter (==card) (getCards to state) in
    if null cands
    then toSimulation state
    else optDecision (ChooseToUse (EffectPut (head cands) (Discard player) (TopOfDeck player))
                                  (\b -> if b then put (head cands) (Discard player) (TopOfDeck player) player state
                                              else toSimulation state))
          player state
royalSealTrigger _ _ _ cont = cont


bank player state = plusMoney (length ts) player state
  where
    ts = filter isTreasure $ inPlay $ playerByName state player

bishop = trashForGain gainPoints &&& eachOtherPlayer mayTrash
  where
    gainPoints card player state = plusTokens (moneyCost (cost state (typ card)) `quot` 2) VictoryToken player state
    mayTrash op state
      | null h = toSimulation state
      | otherwise = optDecision (ChooseCard (EffectTrash unknown (Hand op)) h (\c -> trash c (Hand op) op state)) op state
      where
        h = hand $ playerByName state op

talismanTrigger def player state
  | not (hasType def Victory) && smallerEqCost (cost state def) (simpleCost 4) = gain def player state
  | otherwise = toSimulation state

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

countingHouse :: Action
countingHouse player state
  | null cands = toSimulation state
  | otherwise = chooseMany (EffectPut unknown (Discard player) (Hand player))
                           cands
                           (0,length cands)
                           (\cards -> seqActions (\c -> put c (Discard player) (Hand player)) cards)
                           player state
  where
    cands = filter ((==copper) . typ) $ discardPile $ playerByName state player

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

vault :: Action
vault player state =
  (chooseMany (EffectDiscard unknown (Hand player)) h (0,length h) (\cards -> seqActions (\c -> discard c (Hand player)) cards &&& plusMoney (length cards))
   &&& eachOtherPlayer otherAction)
    player state
  where
    h = hand $ playerByName state player
    otherCont player state cards = (seqActions (\c -> discard c (Hand player)) cards &&& if length cards == 2 then plusCards 1 else pass) player state
    otherAction player state
      | null h2 = toSimulation state
      | otherwise = optDecision (ChooseCards (EffectDiscard unknown (Hand player)) h2 (num,num) (otherCont player state)) player state
      where
        h2 = hand $ playerByName state player
        num = min 2 (length h2)

-- Cornucopia 6xx

cornucopiaCards = map ($ Cornucopia)
  [notImplemented "Hamlet", -- 601 hamlet
   notImplemented "Fortune Teller", -- 602 fortune teller
   action 603 "Menagerie" 3 (plusActions 1 &&& menagerie),
   notImplemented "Farming Village", -- 604 farming village
   notImplemented "Horse Traders", -- 605 horse traders
   notImplemented "Remake", -- 606 remake
   notImplemented "Tournament", -- 607 tournament
   notImplemented "Young Witch", -- 608 young witch
   notImplemented "Harvest", -- 609 harvest
   notImplemented "Horn of Plenty", -- 610 horn of plenty
   notImplemented "Hunting Party", -- 611 hunting party
   notImplemented "Jester", -- 612 jester
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
  [notImplemented "Crossroads", -- 701 crossroads
   notImplemented "Duchess", -- 702 duchess
   notImplemented "Fool's Gold", -- 703 fool's gold
   notImplemented "Develop", -- 704 develop
   action 705 "Oasis" 3 (plusCards 1 &&& plusActions 1 &&& plusMoney 1 &&& discardNCards 1),
   notImplemented "Oracle", -- 706 oracle
   notImplemented "Scheme", -- 707 scheme
   notImplemented "Tunnel", -- 708 tunnel
   action 709 "Jack of All Trades" 4 jackOfAllTrades,
   notImplemented "Noble Brigand", -- 710 noble brigand
   notImplemented "Nomad Camp", -- 711 nomad camp
   withInitialSupply
    (carddef 712 "Silk Road" (simpleCost 4) [Victory] (\p -> length (filter isVictory (allCards p)) `quot` 4) pass noTriggers)
    stdVictorySupply,
   notImplemented "Spice Merchant", -- 713 spice merchant
   notImplemented "Trader", -- 714 trader
   carddef 715 "Cache" (simpleCost 5) [Treasure] (const 0) (plusMoney 3) (onGainSelf (gain copper &&& gain copper)),
   notImplemented "Cartographer", -- 716 cartographer
   notImplemented "Embassy", -- 717 embassy
   notImplemented "Haggler", -- 718 haggler
   actionA 719 "Highway" 5 highway,
   notImplemented "Ill-gotten Gains", -- 720 ill-gotten gains
   notImplemented "Inn", -- 721 inn
   notImplemented "Mandarin", -- 722 mandarin
   notImplemented "Margrave", -- 723 margrave
   notImplemented "Stables", -- 724 stables
   withTrigger (action 725 "Border Village" 6 (plusCards 1 &&& plusActions 2)) (onGainSelf borderVillageTrigger),
   notImplemented "Farmland" -- 726 farmland
   ]

borderVillageTrigger p s = gainUpto ((moneyCost (cost s cBorderVillage)) - 1) p s

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

highway Nothing = plusCards 1 &&& plusActions 1
highway (Just card) =
  plusCards 1 &&& plusActions 1 &&&
  addModifier (ModCost (Just Action))
              (ConditionalModifier (\state -> card `elem` (inPlay $ activePlayer state)) (CappedDecModifier 2))


-- Dark Ages 8xx

darkAgesCards = map ($ DarkAges)
  [notImplemented "Poor House", -- 801 Poor House
   notImplemented "Beggar", -- 802 Beggar
   notImplemented "Squire", -- 803 Squire
   notImplemented "Vagrant", -- 804 Vagrant
   notImplemented "Forager", -- 805 Forager
   notImplemented "Hermit", -- 806 Hermit
   notImplemented "Market Square", -- 807 Market Square
   notImplemented "Sage", -- 808 Sage
   notImplemented "Storeroom", -- 809 Storeroom
   notImplemented "Urchin", -- 810 Urchin
   action 811 "Armory" 4 armoryGain,
   notImplemented "Death Cart", -- 812 Death Cart
   withInitialSupply
     (carddef 813 "Feodum" (simpleCost 4) [Victory] (\p -> length (filter ((==silver) . typ) (allCards p)) `quot` 3)
        pass
        (onTrashSelf (gain silver &&& gain silver &&& gain silver)))
     stdVictorySupply,
   notImplemented "Fortress", -- 814 Fortress
   notImplemented "Ironmonger", -- 815 Ironmonger
   notImplemented "Marauder", -- 816 Marauder
   notImplemented "Procession", -- 817 Procession
   notImplemented "Rats", -- 818 Rats
   notImplemented "Scavenger", -- 819 Scavenger
   notImplemented "Wandering Minstrel", -- 820 Wandering Minstrel
   notImplemented "Band of Misfits", -- 821 Band of Misfits
   notImplemented "Bandit Camp", -- 822 Bandit Camp
   notImplemented "Catacombs", -- 823 Catacombs
   notImplemented "Count", -- 824 Count
   notImplemented "Counterfeit", -- 825 Counterfeit
   notImplemented "Cultist", -- 826 Cultist
   notImplemented "Graverobber", -- 827 Graverobber
   action 828 "Junk Dealer" 5 (plusCards 1 &&& plusActions 1 &&& plusMoney 1 &&& trashNCards 1 1),
   notImplemented "Knights", -- 829 Knights
   notImplemented "Mystic", -- 830 Mystic
   notImplemented "Pillage", -- 831 Pillage
   notImplemented "Rebuild", -- 832 Rebuild
   notImplemented "Rogue", -- 833 Rogue
   notImplemented "Altar", -- 834 Altar
   notImplemented "Hunting Grounds" -- 835 Hunting Grounds
  ]

armoryGain player state = decision
        (ChooseCard (EffectGain unknownDef (TopOfDeck player))
                    (map (`topOfSupply` state) candidates)
                    (\c -> gainTo (typ c) (TopOfDeck player) player state))
        player state
  where
    candidates = affordableCards (simpleCost 4) state


-- Guilds 9xx

guildsCards = map ($ Guilds) [
  action 901 "Candlestick Maker" 2 (plusActions 1 &&& plusBuys 1 &&& plusTokens 1 CoinToken),
  notImplemented "Stonemason", -- 902
  notImplemented "Doctor", -- 903
  notImplemented "Masterpiece", -- 904
  notImplemented "Advisor", -- 905
  notImplemented "Plaza", -- 906
  notImplemented "Taxman", -- 907
  notImplemented "Herald", -- 908
  withTrigger (action 909 "Baker" 5 (plusCards 1 &&& plusActions 1 &&& plusTokens 1 CoinToken))
    (onStartOfGame (\_ state -> foldr (\name sim -> sim `andThen` plusTokens 1 CoinToken name)
                                      (return $ State state)
                                      (map name $ Map.elems $ players state))),
  notImplemented "Butcher",
  notImplemented "Journeyman",
  notImplemented "Merchant Guild",
  notImplemented "Soothsayer"
  ]

-- Adventures 10xx

adventuresCards = map ($ Adventures) [
  notImplemented "Coin of the Realm", -- 1001
  notImplemented "Page", -- 1002
  notImplemented "Peasant", -- 1003
  notImplemented "Ratcatcher", -- 1004
  notImplemented "Raze", -- 1005
  notImplemented "Amulet", -- 1006
  notImplemented "Caravan Guard", -- 1007
  notImplemented "Dungeon", -- 1008
  notImplemented "Gear", -- 1009
  notImplemented "Guide", -- 1010
  notImplemented "Duplicate", -- 1011
  notImplemented "Magpie", -- 1012
  notImplemented "Messenger", -- 1013
  notImplemented "Miser", -- 1014
  withTrigger (action 1015 "Port" 4 (plusCards 1 &&& plusActions 2)) (onBuySelf (gain cPort)),
  notImplemented "Ranger", -- 1016
  notImplemented "Transmogrify", -- 1017
  notImplemented "Artificer", -- 1018
  notImplemented "Bridge Troll", -- 1019
  notImplemented "Distant Lands", -- 1020
  notImplemented "Giant", -- 1021
  notImplemented "Haunted Woods", -- 1022
  notImplemented "Lost City", -- 1023
  notImplemented "Relic", -- 1024
  notImplemented "Royal Carriage", -- 1025
  notImplemented "Storyteller", -- 1026
  notImplemented "Swamp Hag", -- 1027
  carddef 1028 "Treasure Trove" (simpleCost 5) [Treasure] noPoints (plusMoney 2 &&& gain gold &&& gain copper) noTriggers,
  notImplemented "Wine Merchant", -- 1029
  notImplemented "Hireling" -- 1030
  ]

-- Promo 20xx

promoCards = map ($ Promo) []