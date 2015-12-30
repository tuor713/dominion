module Dominion.Cards where

import Dominion.Model

import Data.Char (toLower)
import qualified Data.Either as Either
import qualified Data.List as L
import qualified Data.Map.Strict as Map

kingdomCards :: [CardDef]
kingdomCards = concat [baseCards, intrigueCards, seasideCards, alchemyCards,
                       prosperityCards, cornucopiaCards, hinterlandCards,
                       darkAgesCards, guildsCards, adventuresCards, promoCards]

cardData :: Map.Map String CardDef
cardData = Map.fromList $ map (\c -> (map toLower $ cardName c, c)) (concat [basicCards, kingdomCards, prizes, shelters, ruins])

maybeCard :: String -> Maybe CardDef
maybeCard name = Map.lookup (map toLower name) cardData

lookupCard :: String -> CardDef
lookupCard name = cardData Map.! (map toLower name)

cBeggar          = cardData Map.! "beggar"
cWitch           = cardData Map.! "witch"
cChapel          = cardData Map.! "chapel"
cMilitia         = cardData Map.! "militia"
cSmithy          = cardData Map.! "smithy"
cLibrary         = cardData Map.! "library"
cChancellor      = cardData Map.! "chancellor"
cIsland          = cardData Map.! "island"
cMoat            = cardData Map.! "moat"
cPort            = cardData Map.! "port"
cBorderVillage   = cardData Map.! "border village"
cMinion          = cardData Map.! "minion"
cMint            = cardData Map.! "mint"
cCache           = cardData Map.! "cache"
cNativeVillage   = cardData Map.! "native village"
cGolem           = cardData Map.! "golem"
cTransmute       = cardData Map.! "transmute"
cJackOfAllTrades = cardData Map.! "jack of all trades"
cGardens         = cardData Map.! "gardens"
cWishingWell     = cardData Map.! "wishing well"
cCrossroads      = cardData Map.! "crossroads"
cTrustySteed     = cardData Map.! "trusty steed"
cTournament      = cardData Map.! "tournament"
cTreasureMap     = cardData Map.! "treasure map"

-- Generic action elements (potentially move to model)

revealUntil :: ([Card] -> Bool) -> ([Card] -> Action) -> Action
revealUntil pred cont player state =
  iter 1 state >>= \(s,num) ->
  let cards = take num (deck $ playerByName s player)
  in (reveal cards &&& (cont cards)) player s
  where
    iter :: Int -> GameState -> SimulationT (GameState,Int)
    iter n state = do
      s <- ensureCanDraw n state player
      case s of
        Just ss -> if pred (take n $ deck $ playerByName ss player) then return (ss,n) else iter (n+1) ss
        Nothing -> return (state, n-1)

revealUntilSelector :: (Card -> Bool) -> ([Card] -> [Card] -> Action) -> Action
revealUntilSelector pred cont = revealUntil (any pred) (\cs -> cont (filter pred cs) (filter (not . pred) cs))

eachOtherPlayer :: Action -> Action
eachOtherPlayer action player state = seqSteps action (opponentNames state player) state

playAttack :: Action -> Action
playAttack attack attacker state = seqSteps checkAttack (opponentNames state attacker) state
  where
    checkAttack op state
      | null moats = cont
      | otherwise = decision (ChooseToReact (head moats)
                                            AttackTrigger
                                            (\b -> if b then toSimulation state else cont))
                                            op state
      where
        opponent = playerByName state op
        cont = handleAllTriggers
                AttackTrigger
                (map Left (hand opponent) ++ inPlayDuration opponent)
                NullEffect
                attack op state
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

discardDownTo :: Int -> Action
discardDownTo n op state
  | length h <= n = toSimulation state
  | otherwise = decision (ChooseCards (EffectDiscard unknown (Hand op)) h (length h - n,length h - n) cont) op state
  where
    h = hand $ playerByName state op
    cont cards = discardAll cards (Hand op) op state

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

nameACard :: Effect -> (CardDef -> Action) -> Action
nameACard effect action p s = choose (ChooseCard effect choices)
                                     (\card -> action (typ card))
                                     p s
  where
    choices = map (Card (-1)) (Map.keys $ piles s)


enactEffect :: Effect -> Action
enactEffect (EffectPlusCards no) = plusCards no
enactEffect (EffectPlusActions no) = plusActions no
enactEffect (EffectPlusBuys no) = plusBuys no
enactEffect (EffectPlusMoney no) = plusMoney no
enactEffect (EffectTrashNo no) = trashNCards no no
enactEffect (EffectDiscardNo no) = discardNCards no
enactEffect (EffectGain def to) = gainTo def to
enactEffect (EffectPut card from to) =
  if isUnknown card
  then (\p s -> chooseOne (EffectPut card from to) (hand (playerByName s p)) (\c -> put c from to) p s)
  else put card from to
enactEffect (EffectTrash card from) = trash card from
enactEffect (EffectDiscard card from) = discard card from
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
   attack 111 "Militia" 4 (plusMoney 2 &&& playAttack (discardDownTo 3)),
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
  &&= \cards -> discardAll cards (Hand p) &&& plusCards (length cards)

chapel :: Action
chapel = trashNCards 0 4

deckIntoDiscard :: Action
deckIntoDiscard player state = toSimulation $ updatePlayer state player (\p -> p { deck = [], discardPile = deck p ++ discardPile p })

chancellor :: Action
chancellor player state = plusMoney 2 player state
  `andThen` \s2 -> decision (ChooseToUse (SpecialEffect cChancellor) (cont s2)) player s2
  where
    cont state b = if b then deckIntoDiscard player state else toSimulation state

workshop :: Action
workshop player state =
  (chooseOne (EffectGain unknownDef (Discard player)) (map (`topOfSupply` state) (affordableCardsM 4 state))
   &&= \c -> gain (typ c))
  player state


bureaucrat :: Action
bureaucrat player state = (gainTo silver (TopOfDeck player) &&& playAttack treasureToDeck) player state
  where
    treasureToDeck op state
      | null treasures = reveal h op state
      | otherwise = decision (ChooseCard (EffectPut unknown (Hand op) (TopOfDeck op)) treasures cont) op state
      where
        h = hand $ playerByName state op
        treasures = filter isTreasure h
        cont card = (reveal [card] &&& put card (Hand op) (TopOfDeck op)) op state

feast :: Maybe Card -> Action
feast mCard player state =
  (case mCard of
    Just card -> trash card InPlay player state
    Nothing -> toSimulation state)
  `andThen` \s2 -> decision (ChooseCard (EffectGain unknownDef (Discard player))
                                        (map (`topOfSupply` s2) (affordableCardsM 5 s2))
                                        (\c -> gain (typ c) player s2))
                            player s2

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
   action 209 "Wishing Well" 3 (plusCards 1 &&& plusActions 1 &&& nameACard (SpecialEffect cWishingWell) (\def -> reshuffleIfNeeded &&& wishingWell def)),
   action 210 "Baron" 4 (plusBuys 1 &&& baron),
   action 211 "Bridge" 4 (plusBuys 1 &&& plusMoney 1 &&& addModifier (ModCost Nothing) (CappedDecModifier 1)),
   action 212 "Conspirator" 4 (plusMoney 2 &&& conspirator),
   notImplemented "Coppersmith", -- 213
   action 214 "Ironworks" 4 ironworks,
   actionA 215 "Mining Village" 4 miningVillage,
   notImplemented "Scout", -- 216
   withInitialSupply (carddef 217 "Duke" (simpleCost 5) [Victory] (\p -> length $ filter ((==duchy) . typ) (allCards p)) pass noTriggers)
    stdVictorySupply,
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

conspirator :: Action
conspirator p s
  | actionsThisTurn >= 3 = (plusCards 1 &&& plusActions 1) p s
  | otherwise = toSimulation s
  where
    actionsThisTurn = length $ filter (`hasType` Action) $ playsThisTurn s

courtyard :: Action
courtyard = plusCards 3 &&& putOneBack
  where
    putOneBack :: Action
    putOneBack player state =
      chooseOne (EffectPut unknown (Hand player) (TopOfDeck player))
                (hand (playerByName state player))
                cont
                player
                state
    cont card player = put card (Hand player) (TopOfDeck player) player

ironworks :: Action
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
      (discardAll (hand $ playerByName state player) (Hand player) &&& plusCards 4 &&& playAttack attack) player state
    act [a] player state = enactEffect a player state
    act _ _ _ = error "Minion attack got back other than singleton effect"

    attack op state =
      if length (hand $ playerByName state op) > 4
      then (discardAll (hand $ playerByName state op) (Hand op) &&& (plusCards 4)) op state
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

wishingWell :: CardDef -> Action
wishingWell def p s
  | null d = toSimulation s
  | typ (head d) == def = (reveal [head d] &&& put (head d) (TopOfDeck p) (Hand p)) p s
  | otherwise = reveal [head d] p s
  where
    d = deck player
    player = playerByName s p



-- Seaside 3xx

seasideCards = map ($ Seaside)
  [notImplemented "Embargo", -- 301 Embargo
   notImplemented "Haven", -- 302 Haven
   withTrigger (duration 303 "Lighthouse" 2 (plusActions 1 &&& plusMoney 1) (plusMoney 1))
    (whileInPlay lighthouseProtection),
   action 304 "Native Village" 2 (plusActions 2 &&& nativeVillage),
   action 305 "Pearl Diver" 2 (plusCards 1 &&& plusActions 1 &&& reshuffleIfNeeded &&& pearlDiver),
   notImplemented "Ambassador", -- 306 Ambassador
   duration 307 "Fishing Village" 3 (plusActions 2 &&& plusMoney 1) (plusActions 1 &&& plusMoney 1),
   notImplemented "Lookout", -- 308 Lookout
   notImplemented "Smugglers", -- 309 Smugglers
   action 310 "Warehouse" 3 (plusCards 3 &&& plusActions 1 &&& discardNCards 3),
   duration 311 "Caravan" 4 (plusCards 1 &&& plusActions 1) (plusCards 1),
   attack 312 "Cutpurse" 4 (plusMoney 2 &&& playAttack cutpurse),
   withInitialSupply (carddefA 313 "Island" (simpleCost 4) [Action, Victory] (const 2) island noTriggers) stdVictorySupply,
   notImplemented "Navigator", -- 314 Navigator
   notImplemented "Pirate Ship", -- 315 Pirate Ship
   action 316 "Salvager" 4 (plusBuys 1 &&& trashForGain salvager),
   attack 317 "Sea Hag" 4 (playAttack (reshuffleIfNeeded &&& seaHag)),
   carddefA 318 "Treasure Map" (simpleCost 4) [Action] noPoints treasureMap noTriggers,
   action 319 "Bazaar" 5 (plusCards 1 &&& plusActions 2 &&& plusMoney 1),
   action 320 "Explorer" 5 explorer,
   attack 321 "Ghost Ship" 5 (plusCards 2 &&& playAttack ghostShip),
   duration 322 "Merchant Ship" 5 (plusMoney 2) (plusMoney 2),
   notImplemented "Outpost", -- 323 Outpost
   durationA 324 "Tactician" 5 tactician (plusCards 5 &&& plusBuys 1 &&& plusActions 1),
   withTrigger (action 325 "Treasury" 5 (plusCards 1 &&& plusActions 1 &&& plusMoney 1))
               (onDiscardFromPlay treasuryTrigger),
   duration 326 "Wharf" 5 (plusCards 2 &&& plusBuys 1) (plusCards 2 &&& plusBuys 1)
  ]

cutpurse :: Action
cutpurse op state
  | null cands = reveal h op state
  | otherwise = discard (head cands) (Hand op) op state
  where
    cands = filter ((==copper) . typ) h
    h = hand $ playerByName state op

explorer :: Action
explorer player state =
  chooseToReveal (==province)
                 (gainTo gold (Hand player))
                 (gainTo silver (Hand player))
                 player
                 state

ghostShip :: Action
ghostShip op state
  | numToPut > 0 = chooseMany (EffectPut unknown (Hand op) (TopOfDeck op)) h (numToPut,numToPut)
                    (\cards -> seqActions (\c -> put c (Hand op) (TopOfDeck op)) cards)
                    op state
  | otherwise = toSimulation state
  where
    numToPut = length h - 3
    h = hand $ playerByName state op

island :: Maybe Card -> Action
island mCard player state
  | null h = islandPut player state
  | otherwise = chooseOne (EffectPut unknown (Hand player) (Mat player IslandMat)) h cont player state
  where
    h = hand $ playerByName state player
    cont card = islandPut &&& put card (Hand player) (Mat player IslandMat)
    islandPut = case mCard of
      Just card -> put card InPlay (Mat player IslandMat)
      Nothing -> pass

lighthouseProtection :: TriggerHandler
lighthouseProtection AttackTrigger _ _ _ = pass
lighthouseProtection _ _ _ cont = cont

nativeVillage :: Action
nativeVillage player state = chooseEffects 1 [EffectPut unknown (TopOfDeck player) (Mat player NativeVillageMat),
                                              EffectPut unknown (Mat player NativeVillageMat) (Hand player)]
                                cont player state
  where
    cont [(EffectPut _ (TopOfDeck _) (Mat _ NativeVillageMat))] player state =
      (reshuffleIfNeeded &&& \p s -> let d = deck (playerByName s p) in
                                      if null d then toSimulation s
                                                else put (head d) (TopOfDeck p) (Mat p NativeVillageMat) p s)
      player state


    cont [(EffectPut _ (Mat _ NativeVillageMat) (Hand _))] player state =
      seqActions (\c -> put c (Mat player NativeVillageMat) (Hand player))
                 (getCards (Mat player NativeVillageMat) state)
                 player state
    cont _ _ _ = error "Invalid selection for Native Village"


pearlDiver :: Action
pearlDiver player state
  | null d = toSimulation state
  | otherwise = decision (ChooseToUse (EffectPut (last d) (BottomOfDeck player) (TopOfDeck player))
                                      (\b -> if b then put (last d) (BottomOfDeck player) (TopOfDeck player) player state
                                                  else toSimulation state))
                         player state
  where
    d = deck $ playerByName state player

salvager :: Card -> Action
salvager card player state = plusMoney (moneyCost (cost state (typ card))) player state

seaHag :: Action
seaHag op state
  | null top = gainTo curse (TopOfDeck op) op state
  | otherwise = (discard (head top) (TopOfDeck op) &&& gainTo curse (TopOfDeck op)) op state
  where
    top = getCards (TopOfDeck op) state


tactician :: Maybe Card -> Action
tactician source player state
  | null h = case source of
              -- nothing is going to happen, tactician should be discarded at the end of turn
              (Just card) -> put card InPlayDuration InPlay player state
              Nothing -> toSimulation state
  | otherwise = seqActions (\c -> discard c (Hand player)) h player state
  where
    h = hand $ playerByName state player

treasureMap :: Maybe Card -> Action
treasureMap Nothing p s
  | null ts = toSimulation s
  | otherwise = trash (head ts) (Hand p) p s
  where
    ts = filter ((==cTreasureMap) . typ) $ hand $ playerByName s p

treasureMap (Just c) p s
  | null ts = trash c InPlay p s
  | otherwise = (trash c InPlay &&& trash (head ts) (Hand p)
                 &&& gainTo gold (TopOfDeck p) &&& gainTo gold (TopOfDeck p) &&& gainTo gold (TopOfDeck p) &&& gainTo gold (TopOfDeck p))
                  p s
  where
    ts = filter ((==cTreasureMap) . typ) $ hand $ playerByName s p


treasuryTrigger :: Card -> Action -> Action
treasuryTrigger card cont player state
  | null (filter (`hasType` Victory) buys) =
    decision (ChooseToUse (EffectPut card (Discard player) (TopOfDeck player))
                          (\b -> if b then (cont &&& put card (Discard player) (TopOfDeck player)) player state
                                      else cont player state))
      player state
  | otherwise = cont player state
  where
    buys = buysThisTurn state


-- Alchemy 4xx

alchemyCards = map ($ Alchemy)
  [notImplemented "Herbalist", -- 401 Herbalist
   action 402 "Apprentice" 5 (plusActions 1 &&& trashForGain apprenticeBenefit),
   carddef 403 "Transmute" (fullCost 0 1) [Action] (const 0) (trashForGain transmute) noTriggers,
   withInitialSupply (carddef 404 "Vineyard" (fullCost 0 1) [Victory] (\p -> length (filter isAction (allCards p)) `quot` 3) pass noTriggers)
    stdVictorySupply,
   notImplemented "Apothecary", -- 405 Apothecary -> needs ordering of cards
   notImplemented "Scrying Pool", -- 406 Scrying Pool
   carddef 407 "University" (fullCost 2 1) [Action] (const 0) (plusActions 2 &&& universityGain) noTriggers,
   notImplemented "Alchemist", -- 408 Alchemist
   carddef 409 "Familiar" (fullCost 3 1) [Action, Attack] (const 0) familiar noTriggers,
   carddef 410 "Philosopher's Stone" (fullCost 3 1) [Treasure] (const 0) philosophersStone noTriggers,
   carddef 411 "Golem" (fullCost 4 1) [Action] (const 0) golem noTriggers,
   notImplemented "Possession" -- 412 Possession
  ]

apprenticeBenefit card player state = (if drawNo == 0 then pass else plusCards drawNo) player state
  where
    drawNo = moneyCost c + 2 * potionCost c
    c = cost state (typ card)

familiar :: Action
familiar = plusCards 1 &&& plusActions 1 &&& playAttack (gain curse)

golem :: Action
golem player state = revealUntil ((==2) . length . filter usable) cont player state
  where
    cont cards =
      discardAll (filter (not . usable) cards) (TopOfDeck player)
      &&& (case acts of
            [a,b] -> chooseMany (EffectPlayAction unknown) [a,b] (2,2) (seqActions (\c -> playFrom c (TopOfDeck player)))
            [a] -> playFrom a (TopOfDeck player)
            _ -> pass)
      where
        acts = filter usable cards
    usable c = isAction c && (typ c) /= cGolem


philosophersStone player state = plusMoney num player state
  where
    p = playerByName state player
    num = (length (discardPile p) + length (deck p)) `quot` 5

transmute :: Card -> Action
transmute card = (if isAction card then gain duchy else pass)
                 &&& (if isTreasure card then gain cTransmute else pass)
                 &&& (if isVictory card then gain gold else pass)

universityGain player state =
  optDecision (ChooseCard (EffectGain unknownDef (Discard player)) candidates (\c -> gain (typ c) player state)) player state
  where
    candidates = map (`topOfSupply` state) $ filter (`hasType` Action) $ affordableCardsM 5 state

-- Prosperity 5xx

prosperityCards = map ($ Prosperity)
  [carddef 501 "Loan" (simpleCost 3) [Treasure] noPoints (plusMoney 1 &&& loan) noTriggers,
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
   attack 513 "Mountebank" 5 (plusMoney 2 &&& playAttack mountebank),
   attack 514 "Rabble" 5 (plusCards 3 &&& playAttack (reshuffleIfNeededN 3 &&& rabble)),
   withTrigger (treasure 515 "Royal Seal" 5 2) (whileInPlay royalSealTrigger),
   action 516 "Vault" 5 (plusCards 2 &&& vault),
   carddef 517 "Venture" (simpleCost 5) [Treasure] noPoints (plusMoney 1 &&& venture) noTriggers,
   withTrigger (attack 518 "Goons" 6 (plusBuys 1 &&& plusMoney 2 &&& playAttack (discardDownTo 3)))
    (whileInPlay (onBuy (\_ -> plusTokens 1 VictoryToken))),
   withBuyRestriction
    (action 519 "Grand Market" 6 (plusCards 1 &&& plusActions 1 &&& plusBuys 1 &&& plusMoney 2))
    (\state -> not $ copper `elem` (map typ (inPlay (activePlayer state)))),
   withTrigger (treasure 520 "Hoard" 6 2) (whileInPlay (onBuy (\def -> if hasType def Victory then gain gold else pass))),
   carddef 521 "Bank" (simpleCost 7) [Treasure] noPoints bank noTriggers,
   action 522 "Expand" 7 (remodelX 3),
   action 523 "Forge" 7 forge,
   action 524 "King's Court" 7 kingsCourt,
   withSpecialCost (action 525 "Peddler" 8 (plusCards 1 &&& plusActions 1 &&& plusMoney 1)) peddlerCost
   ]

venture :: Action
venture = revealUntil (any isTreasure) (\cards p s -> (discardAll (filter (not . isTreasure) cards) (TopOfDeck p)
                                                       &&& maybe pass (\t -> playFrom t (TopOfDeck p)) (L.find isTreasure cards))
                                                       p s)

forge :: Action
forge player state = chooseMany (EffectTrash unknown (Hand player)) h (0,length h) cont player state
  where
    h = hand $ playerByName state player
    cont cards
      | null cands = trashAll cards (Hand player)
      | otherwise = trashAll cards (Hand player) &&& chooseOne (EffectGain unknownDef (Discard player)) cands (gain . typ)
      where
        total = simpleCost $ moneyCost $ foldr addCost (simpleCost 0) $ map (cost state . typ) cards
        cands = map (`topOfSupply` state)
          $ filter ((==total) . cost state)
          $ availableCards state

loan :: Action
loan player state = revealUntil (any isTreasure)
                                (\cs -> if any isTreasure cs
                                        then
                                          chooseEffects 1 [EffectDiscard (last cs) (TopOfDeck player), EffectTrash (last cs) (TopOfDeck player)] enactEffects
                                          &&& discardAll (init cs) (TopOfDeck player)
                                        else discardAll cs (TopOfDeck player))
                                player state

mountebank :: Action
mountebank op state
  | null curses = hit op state
  | otherwise = choose (ChooseToUse (EffectDiscard (head curses) (Hand op))) (\b -> if b then discard (head curses) (Hand op) else hit)
                  op state
  where
    hit op state = (gain curse &&& gain copper) op state
    curses = filter ((==curse) . typ) $ hand $ playerByName state op

peddlerCost :: GameState -> Cost
peddlerCost state
  | BuyPhase == phase (turn state) =
    simpleCost $ max 0 (8 - 2 * actionsInPlay)
  | otherwise = simpleCost 8
  where
    p = activePlayer state
    actionsInPlay = length (filter isAction (inPlay p ++ Either.lefts (inPlayDuration p)))

-- TODO how can we do the re-ordering more precisely
rabble :: Action
rabble op state = (reveal top3 &&& discardAll toDiscard (TopOfDeck op) &&& (if length toKeep > 1 then reorder else pass)) op state
  where
    top3 = take 3 $ deck $ playerByName state op
    toDiscard = filter (\c -> isAction c || isTreasure c) top3
    toKeep = filter (\c -> not (isAction c || isTreasure c)) top3
    reorder = chooseMany (EffectPut unknown (TopOfDeck op) (TopOfDeck op))
                         toKeep
                         (length toKeep, length toKeep)
                         (\cs _ s -> toSimulation $ updatePlayer s op (\p -> p { deck = cs ++ drop (length toKeep) (deck p) }))

watchtower :: Action
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
  [action 601 "Hamlet" 2 (plusCards 1 &&& plusActions 1 &&& hamlet),
   attack 602 "Fortune Teller" 3 (plusMoney 2 &&& playAttack fortuneTeller),
   action 603 "Menagerie" 3 (plusActions 1 &&& menagerie),
   action 604 "Farming Village" 4 (plusActions 2 &&& farmingVillage),
   notImplemented "Horse Traders", -- 605 horse traders
   -- TODO should probably be renamed and abstracted
   action 606 "Remake" 4 (trashForGain upgradeBenefit &&& trashForGain upgradeBenefit),
   withTrigger (action 607 "Tournament" 4 (plusActions 1 &&& tournament))
    (onStartOfGame (\_ state -> toSimulation $ state { nonSupplyPiles = Map.fromList (map (\p -> (p, [Card (10000 * (cardTypeId p)) p])) prizes) })),
   notImplemented "Young Witch", -- 608 young witch
   action 609 "Harvest" 5 harvest,
   carddefA 610 "Horn of Plenty" (simpleCost 5) [Treasure] noPoints hornOfPlenty noTriggers,
   action 611 "Hunting Party" 5 (plusCards 1 &&& plusActions 1 &&& huntingParty),
   attack 612 "Jester" 5 (plusMoney 2 &&& jester),
   withInitialSupply (carddef 613 "Fairgrounds" (simpleCost 6) [Victory] fairgroundsPoints pass noTriggers)
    stdVictorySupply
  ]

prizes = map ($ Cornucopia)
  [carddef 620 "Bag of Gold" (simpleCost 0) [Action, Prize] noPoints (\p s -> (plusActions 1 &&& gainTo gold (TopOfDeck p)) p s) noTriggers,
   carddef 621 "Diadem" (simpleCost 0) [Treasure, Prize] noPoints (plusMoney 2 &&& (\p s -> plusMoney (actions (turn s)) p s)) noTriggers,
   carddef 622 "Followers" (simpleCost 0) [Action, Attack, Prize] noPoints
    (plusCards 2 &&& gain estate &&& playAttack (gain curse &&& discardDownTo 3))
    noTriggers,
   carddefA 623 "Princess" (simpleCost 0) [Action, Prize] noPoints princess noTriggers,
   carddef 624 "Trusty Steed" (simpleCost 0) [Action, Prize] noPoints trustySteed noTriggers
   ]

princess :: Maybe Card -> Action
princess Nothing = plusBuys 1
princess (Just card) = plusBuys 1 &&& addModifier (ModCost Nothing) (ConditionalModifier ((card `elem`) . inPlay . activePlayer) (CappedDecModifier 2))

fairgroundsPoints :: Player -> Int
fairgroundsPoints p = 2 * ((length $ L.nub $ map typ $ allCards p) `quot` 5)

farmingVillage :: Action
farmingVillage = revealUntilSelector (\c -> isAction c || isTreasure c)
                                     (\match other p s -> (discardAll other (TopOfDeck p) &&& putAll match (TopOfDeck p) (Hand p)) p s)

fortuneTeller :: Action
fortuneTeller = revealUntilSelector (\c -> typ c == curse || isVictory c) (\_ other p s -> discardAll other (TopOfDeck p) p s)

hamlet :: Action
hamlet = (\p s -> optDecision
                    (ChooseCard (EffectDiscard unknown (Hand p)) (hand (playerByName s p)) (\c -> (discard c (Hand p) &&& plusActions 1) p s))
                    p s)
  &&& (\p s -> optDecision
                (ChooseCard (EffectDiscard unknown (Hand p)) (hand (playerByName s p)) (\c -> (discard c (Hand p) &&& plusBuys 1) p s))
                p s)

harvest :: Action
harvest = revealUntil ((==4) . length) (\cs p s -> (discardAll cs (TopOfDeck p) &&& plusMoney (length cs)) p s)

hornOfPlenty :: Maybe Card -> Action
hornOfPlenty source p s
  | null candidates = toSimulation s
  | otherwise = chooseOne
                  (EffectGain unknownDef (Discard p))
                  (map (`topOfSupply` s) candidates)
                  (\c -> gain (typ c) &&& (if isVictory c then trashHorn source else pass))
                  p s
  where
    candidates = affordableCards (simpleCost typesInPlay) s
    typesInPlay = length $ L.nub $ map typ (inPlay player ++ Either.lefts (inPlayDuration player))
    player = playerByName s p
    trashHorn Nothing = pass
    trashHorn (Just card) = trash card InPlay

huntingParty :: Action
huntingParty p s =
  (reveal h &&&
   revealUntilSelector (\c -> not ((typ c) `elem` htypes))
                       (\match other p s -> (discardAll other (TopOfDeck p) &&& putAll match (TopOfDeck p) (Hand p)) p s))
   p s
  where
    h = hand $ playerByName s p
    htypes = L.nub $ map typ $ h

jester :: Action
jester attacker state = playAttack (reshuffleIfNeeded &&& attack) attacker state
  where
    attack op state
      | null d = toSimulation state
      | isVictory top = gain curse op state
      | otherwise = chooseEffects 1 [EffectGain (typ top) (Discard op), EffectGain (typ top) (Discard attacker)]
                      enact
                      attacker state
      where
        d = deck (playerByName state op)
        top = head d
        enact [(EffectGain _ (Discard p))] _ s = gain (typ top) p s
        enact _ _ _ = error "Unexpected choice from Jester"

menagerie :: Action
menagerie player state = (reveal h &&& (if unique then plusCards 3 else plusCards 1)) player state
  where
    h = hand $ playerByName state player
    cardTypes = map typ h
    unique = length cardTypes == length (L.nub cardTypes)

tournament :: Action
tournament playerId s = allDecisions Map.empty (playerNames s)
  where
    final :: Map.Map PlayerId Bool -> Simulation
    final choices =
      ((if Map.findWithDefault False playerId choices then discard (head (provinces playerId)) (Hand playerId) &&& gainPrize else pass)
       &&& (if Map.size (Map.filterWithKey (\k v -> v && k /= playerId) choices) > 0 then pass else plusCards 1 &&& plusMoney 1))
      playerId s

    provinces player = filter ((==province) . typ) $ hand $ playerByName s player

    gainPrize :: Action
    gainPrize player state = chooseEffects 1 [SpecialEffect cTournament, EffectGain duchy (TopOfDeck player)] (enact . head) player state

    enact :: Effect -> Action
    enact (SpecialEffect _) p s
      | null ps = toSimulation s
      | otherwise = chooseOne (EffectGain unknownDef (TopOfDeck p)) ps (\c -> gainSpecial (typ c) (TopOfDeck p)) p s
      where
        ps = map head $ Map.elems $ Map.filter (not . null) (nonSupplyPiles s)
    enact e p s = enactEffect e p s

    allDecisions :: Map.Map PlayerId Bool -> [PlayerId] -> Simulation
    allDecisions accu [] = final accu
    allDecisions accu (p:ps)
      | null (provinces p) = allDecisions (Map.insert p False accu) ps
      | otherwise = decision (ChooseToUse (EffectReveal (head (provinces p))) (\b -> allDecisions (Map.insert p b accu) ps)) p s

trustySteed :: Action
trustySteed p s = chooseEffects 2 [EffectPlusCards 2, EffectPlusActions 2, EffectPlusMoney 2, SpecialEffect cTrustySteed] enactAll p s
  where
    enact (SpecialEffect _) = gain silver &&& gain silver &&& gain silver &&& gain silver &&& deckIntoDiscard
    enact e = enactEffect e
    enactAll = foldr (\e a -> enact e &&& a) pass


-- Hinterlands 7xx

hinterlandCards = map ($ Hinterlands)
  [action 701 "Crossroads" 2 crossRoads,
   notImplemented "Duchess", -- 702 duchess
   notImplemented "Fool's Gold", -- 703 fool's gold
   notImplemented "Develop", -- 704 develop
   action 705 "Oasis" 3 (plusCards 1 &&& plusActions 1 &&& plusMoney 1 &&& discardNCards 1),
   notImplemented "Oracle", -- 706 oracle
   notImplemented "Scheme", -- 707 scheme
   carddef 708 "Tunnel" (simpleCost 3) [Victory, Reaction] (const 2)
    pass
    (onDiscardSelf tunnelTrigger),
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
   withTrigger (action 717 "Embassy" 5 (plusCards 5 &&& discardNCards 3))
    (onGainSelf (eachOtherPlayer (gain silver))),
   notImplemented "Haggler", -- 718 haggler
   actionA 719 "Highway" 5 highway,
   carddef 720 "Ill-gotten Gains" (simpleCost 5) [Treasure] noPoints
    (\p s -> (plusMoney 1 &&& choose (ChooseToUse (EffectGain copper (Hand p))) (\b -> if b then gainTo copper (Hand p) else pass)) p s)
    (onGainSelf (eachOtherPlayer (gain curse))),
   notImplemented "Inn", -- 721 inn
   withTrigger (action 722 "Mandarin" 5 (plusMoney 3 &&& mandarinAction)) (onGainSelf mandarinTrigger),
   attack 723 "Margrave" 5 (plusCards 3 &&& plusBuys 1 &&& playAttack (plusCards 1 &&& discardDownTo 3)),
   action 724 "Stables" 5 stables,
   withTrigger (action 725 "Border Village" 6 (plusCards 1 &&& plusActions 2)) (onGainSelf borderVillageTrigger),
   notImplemented "Farmland" -- 726 farmland
   ]

borderVillageTrigger :: Action
borderVillageTrigger p s = gainUpto ((moneyCost (cost s cBorderVillage)) - 1) p s

crossRoads :: Action
crossRoads p s = (reveal h
                  &&& (if numVictory > 0 then plusCards numVictory else pass)
                  &&& (if numCrossroads == 1 then plusActions 3 else pass))
                  p s
  where
    h = hand $ playerByName s p
    numVictory = length $ filter isVictory h
    numCrossroads = length $ filter (==cCrossroads) $ playsThisTurn s

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

highway :: Maybe Card -> Action
highway Nothing = plusCards 1 &&& plusActions 1
highway (Just card) = plusCards 1 &&& plusActions 1 &&&
  addModifier (ModCost Nothing) (ConditionalModifier ((card `elem`) . inPlay . activePlayer) (CappedDecModifier 1))

mandarinAction :: Action
mandarinAction player state
  | null h = toSimulation state
  | otherwise = chooseOne (EffectPut unknown (Hand player) (TopOfDeck player)) h (\c -> put c (Hand player) (TopOfDeck player)) player state
  where
    h = hand $ playerByName state player

mandarinTrigger :: Action
mandarinTrigger player state = seqActions (\c -> put c InPlay (TopOfDeck player)) treasures player state
  where
    treasures = filter isTreasure $ inPlay (playerByName state player)

stables :: Action
stables p s
  | null ts = toSimulation s
  | otherwise = optDecision (ChooseCard (EffectDiscard unknown (Hand p)) ts (\card -> (discard card (Hand p) &&& plusCards 3 &&& plusActions 1) p s)) p s
  where
    ts = filter isTreasure $ hand $ playerByName s p

tunnelTrigger :: Card -> Action
tunnelTrigger c p s
  | phase (turn s) == CleanupPhase = toSimulation s
  | otherwise = choose (ChooseToReact c DiscardTrigger) (\b -> if b then reveal [c] &&& gain gold else pass) p s

-- Dark Ages 8xx

darkAgesCards = map ($ DarkAges)
  [action 801 "Poor House" 1 poorHouse,
   carddef 802 "Beggar" (simpleCost 2) [Action, Reaction] noPoints
    (\p -> (gainTo copper (Hand p) &&& gainTo copper (Hand p) &&& gainTo copper (Hand p)) p)
    (whileInHand (onAttackA beggarTrigger)),
   withTrigger
    (action 803 "Squire" 2
      (\p s -> (plusMoney 1 &&& chooseEffects 1 [EffectPlusActions 2, EffectPlusBuys 2, EffectGain silver (Discard p)] enactEffects) p s))
    (onTrashSelf squireTrigger),
   action 804 "Vagrant" 2 (plusCards 1 &&& plusActions 1 &&& reshuffleIfNeeded &&& vagrant),
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
   withTrigger (action 814 "Fortress" 4 (plusCards 1 &&& plusActions 2)) fortressTrigger,
   action 815 "Ironmonger" 4 (plusCards 1 &&& plusActions 1 &&& reshuffleIfNeeded &&& ironmonger),
   notImplemented "Marauder", -- 816 Marauder
   notImplemented "Procession", -- 817 Procession
   notImplemented "Rats", -- 818 Rats
   notImplemented "Scavenger", -- 819 Scavenger
   notImplemented "Wandering Minstrel", -- 820 Wandering Minstrel
   notImplemented "Band of Misfits", -- 821 Band of Misfits
   notImplemented "Bandit Camp", -- 822 Bandit Camp
   notImplemented "Catacombs", -- 823 Catacombs
   action 824 "Count" 5 count,
   notImplemented "Counterfeit", -- 825 Counterfeit
   notImplemented "Cultist", -- 826 Cultist
   notImplemented "Graverobber", -- 827 Graverobber
   action 828 "Junk Dealer" 5 (plusCards 1 &&& plusActions 1 &&& plusMoney 1 &&& trashNCards 1 1),
   notImplemented "Knights", -- 829 Knights
   notImplemented "Mystic", -- 830 Mystic
   notImplemented "Pillage", -- 831 Pillage
   notImplemented "Rebuild", -- 832 Rebuild
   notImplemented "Rogue", -- 833 Rogue
   action 834 "Altar" 6 (trashForGain (\_ -> gainUpto 5)),
   notImplemented "Hunting Grounds" -- 835 Hunting Grounds
  ]

ruins = map ($ DarkAges)
  [carddef 840 "Abandoned Mine" (simpleCost 0) [Action, Ruins] noPoints (plusMoney 1) noTriggers,
   carddef 841 "Ruined Library" (simpleCost 0) [Action, Ruins] noPoints (plusCards 1) noTriggers,
   carddef 842 "Ruined Market" (simpleCost 0) [Action, Ruins] noPoints (plusBuys 1) noTriggers,
   carddef 843 "Ruined Village" (simpleCost 0) [Action, Ruins] noPoints (plusActions 1) noTriggers,
   carddef 844 "Survivors" (simpleCost 0) [Action, Ruins] noPoints (reshuffleIfNeededN 2 &&& survivors) noTriggers
  ]

shelters = map ($ DarkAges)
  [carddef 850 "Hovel" (simpleCost 1) [Reaction, Shelter] noPoints pass (whileInHand (onBuyA hovelTrigger)),
   carddef 851 "Necropolis" (simpleCost 1) [Action, Shelter] noPoints (plusActions 2) noTriggers,
   carddef 852 "Overgrown Estate" (simpleCost 1) [Victory, Shelter] noPoints pass (onTrashSelf (plusCards 1))
   ]


armoryGain :: Action
armoryGain player state = decision
        (ChooseCard (EffectGain unknownDef (TopOfDeck player))
                    (map (`topOfSupply` state) candidates)
                    (\c -> gainTo (typ c) (TopOfDeck player) player state))
        player state
  where
    candidates = affordableCards (simpleCost 4) state

beggarTrigger :: CardLike -> Action
beggarTrigger (Left card) p s = choose (ChooseToUse (EffectDiscard card (Hand p)))
                                       (\b -> if b then discard card (Hand p) &&& gainTo silver (TopOfDeck p) &&& gainTo silver (TopOfDeck p)
                                                   else pass)
                                       p s
beggarTrigger _ p s = pass p s

count :: Action
count player state=
  (chooseEffects 1 [EffectDiscardNo 2, EffectPut unknown (Hand player) (TopOfDeck player), EffectGain copper (Discard player)] enactEffects
   &&& (\p s -> chooseEffects 1 [EffectPlusMoney 3, EffectTrashNo (length (hand (playerByName s p))), EffectGain duchy (Discard player)]
                  enactEffects p s))
    player state


fortressTrigger :: TriggerHandler
fortressTrigger TrashTrigger (EffectTrash c1 _) (Left c2) cont player state =
  if c1 == c2 then (cont &&& put c1 Trash (Hand player)) player state
              else cont player state
fortressTrigger _ _ _ cont p s = cont p s

hovelTrigger :: CardLike -> CardDef -> Action
hovelTrigger (Left hovel) card p s
  | hasType card Victory = choose (ChooseToReact hovel BuyTrigger) (\b -> if b then trash hovel (Hand p) else pass) p s
  | otherwise = toSimulation s
hovelTrigger _ _ _ s = toSimulation s

ironmonger :: Action
ironmonger player state
  | null top = toSimulation state
  | otherwise =
    (reveal [t]
     &&& decision (ChooseToUse (EffectDiscard t (TopOfDeck player)) (\b -> if b then discard t (TopOfDeck player) player state
                                                                                else toSimulation state))
     &&& (if isAction t then plusActions 1 else pass)
     &&& (if isTreasure t then plusMoney 1 else pass)
     &&& (if isVictory t then plusCards 1 else pass))
    player state
  where
    top = getCards (TopOfDeck player) state
    t = head top

poorHouse :: Action
poorHouse = plusMoney 4 &&& \p s ->
  plusMoney (- (min (money (turn s))
                    (length (filter isTreasure (hand (playerByName s p))))))
    p s

squireTrigger :: Action
squireTrigger p s
  | null attacks = toSimulation s
  | otherwise = chooseOne (EffectGain unknownDef (Discard p)) attacks (\c -> gain (typ c)) p s
  where
    attacks = filter isAttack $ map (head . snd) $ Map.toList $ Map.filter (not . null) $ piles s

survivors :: Action
survivors p s
  | null d = toSimulation s
  | length d == 1 = choose (ChooseToUse (EffectDiscard (head d) (TopOfDeck p))) (\b -> if b then discard (head d) (TopOfDeck p) else pass) p s
  | otherwise = info (VisibleToPlayer p) ("Two cards on top: " ++ show c1 ++ ", " ++ show c2) >>
                chooseEffects 1 [EffectDiscard unknown (TopOfDeck p), EffectPut unknown (TopOfDeck p) (TopOfDeck p)] enact p s
  where
    d = deck $ playerByName s p
    [c1,c2] = d
    enact [(EffectDiscard _ _)] = discardAll [c1,c2] (TopOfDeck p)
    enact _ = chooseMany (EffectPut unknown (TopOfDeck p) (TopOfDeck p))
                         [c1,c2]
                         (2,2)
                         (\cs -> putAll (reverse cs) (TopOfDeck p) (TopOfDeck p))

vagrant :: Action
vagrant p s
  | null d = toSimulation s
  | isCurse top && isRuins top && isShelter top && isVictory top = (reveal [top] &&& put top (TopOfDeck p) (Hand p)) p s
  | otherwise = reveal [top] p s
  where
    d = deck $ playerByName s p
    top = head d


-- Guilds 9xx

guildsCards = map ($ Guilds) [
  action 901 "Candlestick Maker" 2 (plusActions 1 &&& plusBuys 1 &&& plusTokens 1 CoinToken),
  notImplemented "Stonemason", -- 902
  notImplemented "Doctor", -- 903
  notImplemented "Masterpiece", -- 904
  notImplemented "Advisor", -- 905
  action 906 "Plaza" 4 (plusCards 1 &&& plusActions 2 &&& plaza),
  notImplemented "Taxman", -- 907
  notImplemented "Herald", -- 908
  withTrigger (action 909 "Baker" 5 (plusCards 1 &&& plusActions 1 &&& plusTokens 1 CoinToken))
    (onStartOfGame (\_ state -> foldr (\name sim -> sim `andThen` plusTokens 1 CoinToken name)
                                      (return $ State state)
                                      (map name $ Map.elems $ players state))),
  notImplemented "Butcher", -- 910
  notImplemented "Journeyman", -- 911
  withTrigger (action 912 "Merchant Guild" 5 (plusBuys 1 &&& plusMoney 1))
    (whileInPlay (onBuy (\_ -> plusTokens 1 CoinToken))),
  notImplemented "Soothsayer" -- 913
  ]

plaza :: Action
plaza p s
  | null ts = toSimulation s
  | otherwise = optDecision (ChooseCard (EffectDiscard unknown (Hand p)) ts (\c -> discard c (Hand p) p s)) p s
  where
    ts = filter isTreasure $ hand $ playerByName s p

-- Adventures 10xx

adventuresCards = map ($ Adventures) [
  notImplemented "Coin of the Realm", -- 1001
  notImplemented "Page", -- 1002
  notImplemented "Peasant", -- 1003
  notImplemented "Ratcatcher", -- 1004
  notImplemented "Raze", -- 1005
  duration 1006 "Amulet" 3 amulet amulet,
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

amulet :: Action
amulet p s = chooseEffects 1 [EffectPlusMoney 1, EffectTrashNo 1, EffectGain silver (Discard p)] enactEffects p s

-- Promo 20xx

promoCards = map ($ Promo) []