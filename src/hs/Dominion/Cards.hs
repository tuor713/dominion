module Dominion.Cards where

import Dominion.Model

import Control.Monad as M
import Data.Char (toLower)
import qualified Data.Either as Either
import qualified Data.List as L
import qualified Data.Map.Strict as Map

kingdomCards :: [CardDef]
kingdomCards = concat [baseCards, intrigueCards, seasideCards, alchemyCards,
                       prosperityCards, cornucopiaCards, hinterlandCards,
                       darkAgesCards, guildsCards, adventuresCards, promoCards]

cardData :: Map.Map String CardDef
cardData = Map.fromList $ map (\c -> (map toLower $ cardName c, c))
            (concat [basicCards, kingdomCards, prizes, shelters, ruins, darkAgesExtra])

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
cDuchess         = cardData Map.! "duchess"
cMystic          = cardData Map.! "mystic"
cProcession      = cardData Map.! "procession"
cSpoils          = cardData Map.! "spoils"
cCounterfeit     = cardData Map.! "counterfeit"
cMarketSquare    = cardData Map.! "market square"
cCultist         = cardData Map.! "cultist"
cRebuild         = cardData Map.! "rebuild"

-- Generic action elements (potentially move to model)

revealUntil :: ([Card] -> Bool) -> ([Card] -> ActionTemplate) -> ActionTemplate
revealUntil pred cont player =
  iter 1 >>= \num ->
  gameState' >>= \state ->
    let cards = take num (deck $ playerByName state player)
    in (reveal cards &&& (cont cards)) player
  where
    iter :: Int -> SimulationT Int
    iter n = do
      reshuffleIfNeededN n player
      s <- gameState'
      if length (deck $ playerByName s player) < n
        then return (n-1)
        else if pred (take n $ deck $ playerByName s player) then return n else iter (n+1)

revealUntilSelector :: (Card -> Bool) -> ([Card] -> [Card] -> ActionTemplate) -> ActionTemplate
revealUntilSelector pred cont = revealUntil (any pred) (\cs -> cont (filter pred cs) (filter (not . pred) cs))

-- reveals N cards (or as much as possible, puts them aside and then invokes X)
revealNAndX :: Int -> ([Card] -> ActionTemplate) -> ActionTemplate
revealNAndX num cont = reshuffleIfNeededN num
  &&& (\p -> gameState' >>= \s ->
               let cs = take num (deck (playerByName s p))
               in ((if null cs then pass else reveal cs) &&& putAll cs (TopOfDeck p) Aside &&& cont cs) p)

eachPlayer :: ActionTemplate -> ActionTemplate
eachPlayer action _ = gameState' >>= \state -> seqSteps action (turnOrder state)

eachOtherPlayer :: ActionTemplate -> ActionTemplate
eachOtherPlayer action player = gameState' >>= \state -> seqSteps action (opponentNames state player)

playAttack :: ActionTemplate -> ActionTemplate
playAttack attack attacker = gameState' >>= \state -> seqSteps (\op -> checkAttack op state) (opponentNames state attacker)
  where
    checkAttack op state
      | null moats = cont
      | otherwise = decision (ChooseToReact (head moats)
                                            AttackTrigger
                                            (\b -> if b then noOpSimulation else cont))
                                            op
      where
        opponent = playerByName state op
        cont = handleAllTriggers
                AttackTrigger
                (map (`FromCard` (Hand op)) (hand opponent) ++ map triggerSource (inPlayDuration opponent))
                NullEffect
                attack op
        moats = filter ((==cMoat) . typ) $ hand $ playerByName state op
        triggerSource (Left card) = FromCard card InPlay
        triggerSource (Right def) = FromCardEffect def


trashNCards :: Int -> Int -> ActionTemplate
trashNCards inmin inmax player = gameState' >>= inner
  where
    inner state = decision (ChooseCards (EffectTrash unknown (Hand player)) candidates (min',inmax) cont) player
      where
        candidates = hand (playerByName state player)
        min' = min (length candidates) inmin
        cont cards = seqActions (\c -> trash c (Hand player)) cards player

discardNCards :: Int -> ActionTemplate
discardNCards num player = gameState' >>= inner
  where
    inner state = decision (ChooseCards (EffectDiscard unknown (Hand player)) candidates (num',num') cont) player
      where
        candidates = hand (playerByName state player)
        num' = min (length candidates) num
        cont cards = seqActions (\c -> discard c (Hand player)) cards player

discardDownTo :: Int -> ActionTemplate
discardDownTo n op = gameState' >>= inner
  where
    inner state
      | length h <= n = noOpSimulation
      | otherwise = decision (ChooseCards (EffectDiscard unknown (Hand op)) h (length h - n,length h - n) cont) op
      where
        h = hand $ playerByName state op
        cont cards = discardAll cards (Hand op) op

trashForGain :: (Card -> ActionTemplate) -> ActionTemplate
trashForGain gain player = gameState' >>= inner
  where
    inner state
      | null h = noOpSimulation
      | otherwise = decision (ChooseCard (EffectTrash unknown (Hand player)) h cont) player
      where
        h = hand $ playerByName state player
        cont card = (trash card (Hand player) &&& gain card) player

gainUptoFiltered :: Int -> (CardDef -> Bool) -> ActionTemplate
gainUptoFiltered x pred player = gameState' >>= inner
  where
    inner state
      | null candidates = noOpSimulation
      | otherwise =
        decision
          (ChooseCard (EffectGain unknownDef (Discard player))
            (map (`topOfSupply` state) candidates)
            (\c -> gain (typ c) player))
          player
      where
        candidates = filter pred $ (affordableCards (simpleCost x) state)

gainUpto :: Int -> ActionTemplate
gainUpto x = gainUptoFiltered x (const True)

gainUpgradeTo :: Int -> Location -> Card -> ActionTemplate
gainUpgradeTo delta target trashed player = gameState' >>= inner
  where
    inner state
      | null candidates = noOpSimulation
      | otherwise = chooseOne (EffectGain unknownDef target) candidates (\c -> gainTo (typ c) target) player
      where
        exactCost = addCost (cost state (typ trashed)) (simpleCost delta)
        candidates = map (`topOfSupply` state)
          $ filter ((==exactCost) . cost state)
          $ availableCards state

gainUpgrade :: Int -> Card -> ActionTemplate
gainUpgrade delta trashed player = gainUpgradeTo delta (Discard player) trashed player

remodelX :: Int -> ActionTemplate
remodelX x = trashForGain chooseToGain
  where
    chooseToGain :: Card -> ActionTemplate
    chooseToGain trashed player =
      gameState' >>= \s2 ->
      decision
        (ChooseCard (EffectGain unknownDef (Discard player))
                    (map (`topOfSupply` s2) (candidates trashed s2))
                    (\c -> gain (typ c) player))
        player
    candidates trashed state = affordableCards (addCost (cost state (typ trashed)) (simpleCost x)) state

nameACard :: Effect -> (CardDef -> ActionTemplate) -> ActionTemplate
nameACard effect action p = do
  s <- gameState'
  let choices = map (Card (-1)) (Map.keys $ piles s)
  choose (ChooseCard effect choices)
         (\card -> action (typ card))
         p



enactEffect :: Effect -> ActionTemplate
enactEffect (EffectPlusCards no) = plusCards no
enactEffect (EffectPlusActions no) = plusActions no
enactEffect (EffectPlusBuys no) = plusBuys no
enactEffect (EffectPlusMoney no) = plusMoney no
enactEffect (EffectTrashNo no) = trashNCards no no
enactEffect (EffectDiscardNo no) = discardNCards no
enactEffect (EffectGain def to) = gainTo def to
enactEffect (EffectPut card from to) =
  if isUnknown card
  then (\p -> gameState' >>= \s -> chooseOne (EffectPut card from to) (hand (playerByName s p)) (\c -> put c from to) p)
  else put card from to
enactEffect (EffectTrash card from) = trash card from
enactEffect (EffectDiscard card from) = discard card from
enactEffect (MultiEffect effects) = enactEffects effects
enactEffect _ = error "Effect not implemented"

enactEffects :: [Effect] -> ActionTemplate
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

cellar :: ActionTemplate
cellar = plusActions 1
  &&+ \p s -> chooseMany (EffectDiscard unknown (Hand p)) (hand (playerByName s p)) (0,length $ (hand (playerByName s p)))
  &&= \cards -> discardAll cards (Hand p) &&& plusCards (length cards)

chapel :: ActionTemplate
chapel = trashNCards 0 4

deckIntoDiscard :: ActionTemplate
deckIntoDiscard player = modGameState' $ \state -> updatePlayer state player (\p -> p { deck = [], discardPile = deck p ++ discardPile p })

chancellor :: ActionTemplate
chancellor player = plusMoney 2 player >> decision (ChooseToUse (SpecialEffect cChancellor) cont) player
  where
    cont b = if b then deckIntoDiscard player else noOpSimulation

workshop :: ActionTemplate
workshop player =
  gameState' >>= \state ->
  (chooseOne (EffectGain unknownDef (Discard player)) (map (`topOfSupply` state) (affordableCardsM 4 state))
   &&= \c -> gain (typ c))
  player


bureaucrat :: ActionTemplate
bureaucrat player = (gainTo silver (TopOfDeck player) &&& playAttack (\op -> gameState' >>= \state -> treasureToDeck op state)) player
  where
    treasureToDeck op state
      | null treasures = reveal h op
      | otherwise = decision (ChooseCard (EffectPut unknown (Hand op) (TopOfDeck op)) treasures cont) op
      where
        h = hand $ playerByName state op
        treasures = filter isTreasure h
        cont card = (reveal [card] &&& put card (Hand op) (TopOfDeck op)) op

feast :: Maybe Card -> ActionTemplate
feast mCard player =
  (case mCard of
    Just card -> trash card InPlay player
    Nothing -> noOpSimulation)
  >> gameState'
  >>= \s2 -> decision (ChooseCard (EffectGain unknownDef (Discard player))
                                        (map (`topOfSupply` s2) (affordableCardsM 5 s2))
                                        (\c -> gain (typ c) player))
                            player

moneylender :: ActionTemplate
moneylender player = gameState' >>= inner
  where
    inner state
      | null coppers = noOpSimulation
      | otherwise = (trash (head coppers) (Hand player) &&& plusMoney 3) player
      where
        coppers = filter ((==copper) . typ) $ hand $ playerByName state player

spy :: ActionTemplate
spy player = (plusCards 1 &&& plusActions 1 &&& spyAction &&& playAttack spyAction) player
  where
    spyAction attackee = do
      reshuffleIfNeeded attackee
      state <- gameState'
      if null (deck $ playerByName state attackee)
        then noOpSimulation
        else doSpy attackee state
    doSpy attackee state =
      (addLog (LogReveal attackee [top]) &&& decision (ChooseToUse (EffectDiscard top (TopOfDeck player)) cont)) player
      where
        cont b = if b then discard top (TopOfDeck attackee) attackee else noOpSimulation
        top = head $ deck $ playerByName state attackee

thief :: ActionTemplate
thief player = playAttack doThief player
  where
    doThief :: ActionTemplate
    doThief op = do
      reshuffleIfNeededN 2 op
      state <- gameState'
      if null (deck $ playerByName state op)
        then noOpSimulation
        else decide op state
    decide op state
      | null (filter isTreasure top) = seqActions (\c -> discard c (TopOfDeck op)) top op
      | otherwise = decision (ChooseCard (EffectTrash unknown (TopOfDeck op)) (filter isTreasure top) cont) player
      where
        top = take 2 $ hand $ playerByName state op
        cont card = trash card (TopOfDeck op) op
          >> (decision (ChooseToUse (EffectGainFrom card Trash (Hand player)) (\b -> if b then gainFrom card Trash player else noOpSimulation)) player)
          >> (seqActions (\c -> discard c (TopOfDeck op)) (L.delete card top)) op

throneRoom :: ActionTemplate
throneRoom player = gameState' >>= inner
  where
    inner state
      | null actions = noOpSimulation
      | otherwise = decision (ChooseCard (EffectPlayCopy unknown) actions cont) player
      where
        actions = filter isAction (hand (playerByName state player))
        cont card = (play card &&& playEffect (typ card) Nothing) player

councilRoom :: ActionTemplate
councilRoom player = (plusCards 4 &&& plusBuys 1) player >> gameState' >>= \state -> seqSteps (plusCards 1) (opponentNames state player)

library :: [Card] -> ActionTemplate
library aside name = gameState' >>= inner
  where
    inner state
      | length (hand (playerByName state name)) >= 7 = finalState
      | otherwise = do
        reshuffleIfNeeded name
        if null (deck $ playerByName state name)
          then finalState
          else draw1 state
      where
        finalState = modGameState' $ \state -> updatePlayer state name (\p -> p { discardPile = aside ++ discardPile p})
        draw1 s2
          | not (canDraw (activePlayer s2)) = finalState
          | isAction card = decision (ChooseToUse (EffectDiscard card (TopOfDeck name)) cont) name
          | otherwise = next
          where
            cont False = next
            cont True = putGameState' (updatePlayer s2 name (\p -> p { deck = tail (deck p) })) >> library (card:aside) name
            next = (plusCards 1 &&& library aside) name
            card = head $ deck $ activePlayer s2

mine :: ActionTemplate
mine player = gameState' >>= inner
  where
    inner state
      | treasures == [] = noOpSimulation
      | otherwise = decision (ChooseCard (EffectTrash unknown (Hand player)) treasures cont) player
      where
        cont card = trash card (Hand player) player >> gainDecision card
        gainDecision trashed = gameState' >>= \s2 -> decision (ChooseCard (EffectGain unknownDef (Hand player)) (affordable trashed s2)
                                                       (\c -> gainTo (typ c) (Hand player) player))
                                                       player
        affordable trashed state =
            filter isTreasure
            $ map (`topOfSupply` state)
            $ affordableCards (addCost (cost state (typ trashed)) (simpleCost 3)) state
        treasures = filter isTreasure (hand (playerByName state player))

witch :: ActionTemplate
witch = plusCards 2 &&& playAttack (gain curse)

-- TODO all home grown primitives rather than reusable pieces
adventurer :: [Card] -> [Card] -> ActionTemplate
adventurer treasures others player
  | length treasures >= 2 = terminal
  | otherwise = do
    reshuffleIfNeeded player
    state <- gameState'
    if null (deck $ playerByName state player)
      then terminal
      else draw1 state
  where
    -- TODO this should probably trigger discard
    terminal = modGameState'$ \state -> updatePlayer state player (\p -> p { hand = treasures ++ hand p, discardPile = others ++ discardPile p })
    draw1 state = do
      putGameState' s2
      adventurer (if isTreasure top then top:treasures else treasures)
                             (if isTreasure top then others else top:others)
                             player
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
   action 216 "Scout" 4 (plusActions 1 &&& revealNAndX 4 scout),
   withInitialSupply (carddef 217 "Duke" (simpleCost 5) [Victory] (\p -> length $ filter ((==duchy) . typ) (allCards p)) pass noTriggers)
    stdVictorySupply,
   attack 218 "Minion" 5 (plusActions 1 &&& minionAttack),
   notImplemented "Saboteur", -- 219 saboteur
   action 220 "Torturer" 5 (plusCards 3 &&& playAttack torturerAttack),
   action 221 "Trading Post" 5 tradingPost,
   notImplemented "Tribute", -- 222 tribute
   action 223 "Upgrade" 5 (plusCards 1 &&& plusActions 1 &&& trashForGain (gainUpgrade 1)),
   withInitialSupply (carddef 224 "Harem" (simpleCost 6) [Treasure, Victory] (const 2) (plusMoney 2) noTriggers)
    stdVictorySupply,
   withInitialSupply (carddef 225 "Nobles" (simpleCost 6) [Action, Victory] (const 2)
                      (chooseEffects 1 [EffectPlusCards 3, EffectPlusActions 2] enactEffects)
                      noTriggers)
                     stdVictorySupply
   ]

baron :: ActionTemplate
baron player = gameState' >>= inner
  where
    inner state
      | not (null estates) =
        decision (ChooseToUse (EffectDiscard (head estates) (Hand player))
                                 (\b -> if b
                                        then (discard (head estates) (Hand player) &&& plusMoney 4) player
                                        else gain estate player))
                    player
      | otherwise = gain estate player
      where
        h = hand $ playerByName state player
        estates = filter ((==estate) . typ) h

conspirator :: ActionTemplate
conspirator p = gameState' >>= inner
  where
    inner s
      | actionsThisTurn >= 3 = (plusCards 1 &&& plusActions 1) p
      | otherwise = noOpSimulation
      where
        actionsThisTurn = length $ filter (`hasType` Action) $ playsThisTurn s

courtyard :: ActionTemplate
courtyard = plusCards 3 &&& putOneBack
  where
    putOneBack :: ActionTemplate
    putOneBack player = gameState' >>= \state ->
      chooseOne (EffectPut unknown (Hand player) (TopOfDeck player))
                (hand (playerByName state player))
                cont
                player
    cont card player = put card (Hand player) (TopOfDeck player) player

ironworks :: ActionTemplate
ironworks player = gameState' >>= \state ->
  (chooseOne (EffectGain unknownDef (Discard player)) (map (`topOfSupply` state) (affordableCardsM 4 state))
   &&= \c -> gain (typ c) &&& ironBenefits c)
  player
  where
    ironBenefits card =
      (if isAction card then plusActions 1 else pass)
      &&& (if isTreasure card then plusMoney 1 else pass)
      &&& (if isVictory card then plusCards 1 else pass)

masquerade player = gameState' >>= \state -> (iter (turnOrder state ++ [head $ turnOrder state]) &&& trashNCards 0 1) player
  where
    iter :: [PlayerId] -> ActionTemplate
    iter (a:b:xs) player =
      gameState' >>= \state ->
      decision (ChooseCard (EffectPut unknown (Hand a) (Hand b))
                           (hand $ playerByName state a)
                           (\card -> (iter (b:xs) &&& put card (Hand a) (Hand b)) player))
               a
    iter _ _ = noOpSimulation



miningVillage Nothing = plusCards 1 &&& plusActions 2
miningVillage (Just card) =
  plusCards 1
  &&& plusActions 2
  &&& (\player -> optDecision (ChooseToUse (EffectTrash card InPlay) (cont player)) player)
  where
    cont _ False = noOpSimulation
    cont player True = (trash card InPlay &&& plusMoney 2) player

minionAttack = chooseEffects 1 [EffectPlusMoney 2, SpecialEffect cMinion] act
  where
    act [SpecialEffect _] player = gameState' >>= \state ->
      (discardAll (hand $ playerByName state player) (Hand player) &&& plusCards 4 &&& playAttack attack) player
    act [a] player = enactEffect a player
    act _ _ = error "Minion attack got back other than singleton effect"

    attack op = gameState' >>= \state ->
      if length (hand $ playerByName state op) > 4
      then (discardAll (hand $ playerByName state op) (Hand op) &&& (plusCards 4)) op
      else noOpSimulation

scout :: [Card] -> ActionTemplate
scout cards p =
  (putAll vs Aside (Hand p)
   &&& (case nonVs of
        [] -> pass
        [c] -> put c Aside (TopOfDeck p)
        cs -> chooseMany (EffectPut unknown Aside (TopOfDeck p)) cs (length cs, length cs)
                         (\order -> putAll (reverse order) Aside (TopOfDeck p))))
    p
  where
    vs = filter isVictory cards
    nonVs = filter (not . isVictory) cards

shantyDraw player = do
  state <- gameState'
  let h = hand (playerByName state player)
      noActions = not $ any isAction h
  (reveal h &&& if noActions then plusCards 2 else pass) player

torturerAttack op = chooseEffects 1 [EffectDiscardNo 2, EffectGain curse (Hand op)] enactEffects op

-- TODO what if a trigger replaces the trashing?
tradingPost player = gameState' >>= inner
  where
    inner state
      | length h >= 2 = (trashNCards 2 2 &&& gainTo silver (Hand player)) player
      | otherwise = trashNCards 2 2 player
      where
        h = hand (playerByName state player)

wishingWell :: CardDef -> ActionTemplate
wishingWell def p = gameState' >>= inner
  where
    inner s
      | null d = noOpSimulation
      | typ (head d) == def = (reveal [head d] &&& put (head d) (TopOfDeck p) (Hand p)) p
      | otherwise = reveal [head d] p
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

cutpurse :: ActionTemplate
cutpurse op = gameState' >>= inner
  where
    inner state
      | null cands = reveal h op
      | otherwise = discard (head cands) (Hand op) op
      where
        cands = filter ((==copper) . typ) h
        h = hand $ playerByName state op

explorer :: ActionTemplate
explorer player =
  chooseToReveal (==province)
                 (gainTo gold (Hand player))
                 (gainTo silver (Hand player))
                 player

ghostShip :: ActionTemplate
ghostShip op = gameState' >>= inner
  where
    inner state
      | numToPut > 0 = chooseMany (EffectPut unknown (Hand op) (TopOfDeck op)) h (numToPut,numToPut)
                        (\cards -> seqActions (\c -> put c (Hand op) (TopOfDeck op)) cards)
                        op
      | otherwise = noOpSimulation
      where
        numToPut = length h - 3
        h = hand $ playerByName state op

island :: Maybe Card -> ActionTemplate
island mCard player = gameState' >>= inner
  where
    inner state
      | null h = islandPut player
      | otherwise = chooseOne (EffectPut unknown (Hand player) (Mat player IslandMat)) h cont player
      where
        h = hand $ playerByName state player
        cont card = islandPut &&& put card (Hand player) (Mat player IslandMat)
        islandPut = case mCard of
          Just card -> put card InPlay (Mat player IslandMat)
          Nothing -> pass

lighthouseProtection :: TriggerHandler
lighthouseProtection AttackTrigger _ _ _ = pass
lighthouseProtection _ _ _ cont = cont

nativeVillage :: ActionTemplate
nativeVillage player = chooseEffects 1 [EffectPut unknown (TopOfDeck player) (Mat player NativeVillageMat),
                                              EffectPut unknown (Mat player NativeVillageMat) (Hand player)]
                                cont player
  where
    cont [(EffectPut _ (TopOfDeck _) (Mat _ NativeVillageMat))] player =
      (reshuffleIfNeeded &&& \p -> gameState' >>= \s ->
                                     let d = deck (playerByName s p) in
                                       if null d then noOpSimulation
                                                 else put (head d) (TopOfDeck p) (Mat p NativeVillageMat) p )
      player


    cont [(EffectPut _ (Mat _ NativeVillageMat) (Hand _))] player =
      gameState' >>= \state ->
      seqActions (\c -> put c (Mat player NativeVillageMat) (Hand player))
                 (getCards (Mat player NativeVillageMat) state)
                 player
    cont _ _ = error "Invalid selection for Native Village"


pearlDiver :: ActionTemplate
pearlDiver player = gameState' >>= inner
  where
    inner state
      | null d = noOpSimulation
      | otherwise = decision (ChooseToUse (EffectPut (last d) (BottomOfDeck player) (TopOfDeck player))
                                          (\b -> if b then put (last d) (BottomOfDeck player) (TopOfDeck player) player
                                                      else noOpSimulation))
                             player
      where
        d = deck $ playerByName state player

salvager :: Card -> ActionTemplate
salvager card player = gameState' >>= \state -> plusMoney (moneyCost (cost state (typ card))) player

seaHag :: ActionTemplate
seaHag op = gameState' >>= inner
  where
    inner state
      | null top = gainTo curse (TopOfDeck op) op
      | otherwise = (discard (head top) (TopOfDeck op) &&& gainTo curse (TopOfDeck op)) op
      where
        top = getCards (TopOfDeck op) state


tactician :: Maybe Card -> ActionTemplate
tactician source player = gameState' >>= inner
  where
    inner state
      | null h = case source of
                  -- nothing is going to happen, tactician should be discarded at the end of turn
                  (Just card) -> put card InPlayDuration InPlay player
                  Nothing -> noOpSimulation
      | otherwise = seqActions (\c -> discard c (Hand player)) h player
      where
        h = hand $ playerByName state player

treasureMap :: Maybe Card -> ActionTemplate
treasureMap Nothing p = gameState' >>= inner
  where
    inner s
      | null ts = noOpSimulation
      | otherwise = trash (head ts) (Hand p) p
      where
        ts = filter ((==cTreasureMap) . typ) $ hand $ playerByName s p

treasureMap (Just c) p = gameState' >>= inner
  where
    inner s
      | null ts = trash c InPlay p
      | otherwise = (trash c InPlay &&& trash (head ts) (Hand p)
                     &&& gainTo gold (TopOfDeck p) &&& gainTo gold (TopOfDeck p) &&& gainTo gold (TopOfDeck p) &&& gainTo gold (TopOfDeck p))
                      p
      where
        ts = filter ((==cTreasureMap) . typ) $ hand $ playerByName s p


treasuryTrigger :: Card -> ActionTemplate -> ActionTemplate
treasuryTrigger card cont player = gameState' >>= inner
  where
    inner state
      | null (filter (`hasType` Victory) buys) =
        decision (ChooseToUse (EffectPut card (Discard player) (TopOfDeck player))
                              (\b -> if b then (cont &&& put card (Discard player) (TopOfDeck player)) player
                                          else cont player))
          player
      | otherwise = cont player
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

apprenticeBenefit card player =
  gameState' >>= \state ->
    let drawNo = moneyCost c + 2 * potionCost c
        c = cost state (typ card)
    in (if drawNo == 0 then pass else plusCards drawNo) player

familiar :: ActionTemplate
familiar = plusCards 1 &&& plusActions 1 &&& playAttack (gain curse)

golem :: ActionTemplate
golem player = revealUntil ((==2) . length . filter usable) cont player
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


philosophersStone player = do
  state <- gameState'
  let p = playerByName state player
      num = (length (discardPile p) + length (deck p)) `quot` 5
  plusMoney num player


transmute :: Card -> ActionTemplate
transmute card = (if isAction card then gain duchy else pass)
                 &&& (if isTreasure card then gain cTransmute else pass)
                 &&& (if isVictory card then gain gold else pass)

universityGain player = do
  state <- gameState'
  let candidates = map (`topOfSupply` state) $ filter (`hasType` Action) $ affordableCardsM 5 state
  optDecision (ChooseCard (EffectGain unknownDef (Discard player)) candidates (\c -> gain (typ c) player)) player


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

venture :: ActionTemplate
venture = revealUntil (any isTreasure) (\cards p -> (discardAll (filter (not . isTreasure) cards) (TopOfDeck p)
                                                       &&& maybe pass (\t -> playFrom t (TopOfDeck p)) (L.find isTreasure cards))
                                                       p)

forge :: ActionTemplate
forge player = gameState' >>= inner
  where
    inner state = chooseMany (EffectTrash unknown (Hand player)) h (0,length h) cont player
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

loan :: ActionTemplate
loan player = revealUntil (any isTreasure)
                                (\cs -> if any isTreasure cs
                                        then
                                          chooseEffects 1 [EffectDiscard (last cs) (TopOfDeck player), EffectTrash (last cs) (TopOfDeck player)] enactEffects
                                          &&& discardAll (init cs) (TopOfDeck player)
                                        else discardAll cs (TopOfDeck player))
                                player

mountebank :: ActionTemplate
mountebank op = gameState' >>= inner
  where
    inner state
      | null curses = hit op
      | otherwise = choose (ChooseToUse (EffectDiscard (head curses) (Hand op))) (\b -> if b then discard (head curses) (Hand op) else hit) op
      where
        hit op = (gain curse &&& gain copper) op
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
rabble :: ActionTemplate
rabble op = gameState' >>= inner
  where
    inner state = (reveal top3 &&& discardAll toDiscard (TopOfDeck op) &&& (if length toKeep > 1 then reorder else pass)) op
      where
        top3 = take 3 $ deck $ playerByName state op
        toDiscard = filter (\c -> isAction c || isTreasure c) top3
        toKeep = filter (\c -> not (isAction c || isTreasure c)) top3
        reorder = chooseMany (EffectPut unknown (TopOfDeck op) (TopOfDeck op))
                             toKeep
                             (length toKeep, length toKeep)
                             (\cs _ -> modGameState' (\s -> updatePlayer s op (\p -> p { deck = cs ++ drop (length toKeep) (deck p) })))

watchtower :: ActionTemplate
watchtower player = gameState' >>= inner
  where
    inner state
      | length h >= 6 = noOpSimulation
      | otherwise = plusCards (6 - length h) player
      where
        h = hand $ playerByName state player

watchtowerReplay :: CardDef -> Location -> ActionTemplate
watchtowerReplay def loc player = gameState' >>= inner
  where
    inner state
      | null cands = noOpSimulation
      | otherwise = chooseEffects 1 [EffectTrash (head cands) loc, EffectPut (head cands) loc (TopOfDeck player)] enactEffects player
      where
        cands = filter ((==def) . typ) $ getCards loc state

watchtowerTrigger :: TriggerHandler
watchtowerTrigger GainTrigger (EffectGain def to) (FromCard w _) cont =
  cont &&& reveal [w] &&& \player -> decision (ChooseToReact w GainTrigger (\b -> if b then watchtowerReplay def to player else noOpSimulation)) player
watchtowerTrigger GainTrigger (EffectGainFrom card _ to) (FromCard w _) cont =
  cont &&& reveal [w] &&& \player -> decision (ChooseToReact w GainTrigger (\b -> if b then watchtowerReplay (typ card) to player else noOpSimulation)) player
watchtowerTrigger _ _ _ cont = cont

royalSealTrigger :: TriggerHandler
royalSealTrigger GainTrigger (EffectGain def to) _ cont =
  cont &&& \player -> gameState' >>= \state ->
    let cands = filter ((==def) . typ) (getCards to state) in
    if null cands
    then noOpSimulation
    else optDecision (ChooseToUse (EffectPut (head cands) (Discard player) (TopOfDeck player))
                                  (\b -> if b then put (head cands) (Discard player) (TopOfDeck player) player
                                              else noOpSimulation))
          player
royalSealTrigger GainTrigger (EffectGainFrom card _ to) _ cont =
  cont &&& \player -> gameState' >>= \state ->
    let cands = filter (==card) (getCards to state) in
    if null cands
    then noOpSimulation
    else optDecision (ChooseToUse (EffectPut (head cands) (Discard player) (TopOfDeck player))
                                  (\b -> if b then put (head cands) (Discard player) (TopOfDeck player) player
                                              else noOpSimulation))
          player
royalSealTrigger _ _ _ cont = cont


bank player = do
  state <- gameState'
  let ts = filter isTreasure $ inPlay $ playerByName state player
  plusMoney (length ts) player

bishop = trashForGain gainPoints &&& eachOtherPlayer mayTrash
  where
    gainPoints card player = gameState' >>= \state -> plusTokens (moneyCost (cost state (typ card)) `quot` 2) VictoryToken player
    mayTrash op = gameState' >>= inner
      where
        inner state
          | null h = noOpSimulation
          | otherwise = optDecision (ChooseCard (EffectTrash unknown (Hand op)) h (\c -> trash c (Hand op) op)) op
          where
            h = hand $ playerByName state op

talismanTrigger def player = gameState' >>= inner
  where
    inner state
      | not (hasType def Victory) && smallerEqCost (cost state def) (simpleCost 4) = gain def player
      | otherwise = noOpSimulation

quarryEffect Nothing = plusMoney 1
quarryEffect (Just card) =
  plusMoney 1 &&&
  addModifier (ModCost (Just Action)) (ConditionalModifier (\state -> card `elem` (inPlay $ activePlayer state)) (CappedDecModifier 2))

city :: ActionTemplate
city = plusCards 1
  &&& plusActions 2
  &&& \player -> gameState' >>= \state -> let num = length $ filter ((==0) . snd) $ supply state in extra num player
  where
    extra 0 = pass
    extra 1 = plusCards 1
    extra _ = plusCards 1 &&& plusMoney 1 &&& plusBuys 1

countingHouse :: ActionTemplate
countingHouse player = gameState' >>= inner
  where
    inner state
      | null cands = noOpSimulation
      | otherwise = chooseMany (EffectPut unknown (Discard player) (Hand player))
                               cands
                               (0,length cands)
                               (\cards -> seqActions (\c -> put c (Discard player) (Hand player)) cards)
                               player
      where
        cands = filter ((==copper) . typ) $ discardPile $ playerByName state player

mintAction :: ActionTemplate
mintAction player = gameState' >>= inner
  where
    inner state
      | null treasures = noOpSimulation
      | otherwise = optDecision (ChooseCard (EffectGain unknownDef (Discard player)) treasures (\card -> gain (typ card) player)) player
      where
        treasures = filter isTreasure $ hand (playerByName state player)

mintTrigger :: ActionTemplate
mintTrigger player = do
  state <- gameState'
  let treasures = filter isTreasure $ inPlay (playerByName state player)
  trashAll treasures InPlay player

kingsCourt :: ActionTemplate
kingsCourt player = gameState' >>= inner
  where
    inner state
      | actions == [] = noOpSimulation
      | otherwise = decision (ChooseCard (EffectPlayCopy unknown) actions cont) player
      where
        actions = filter isAction (hand (playerByName state player))
        cont card = (play card &&& playEffect (typ card) Nothing &&& playEffect (typ card) Nothing) player

vault :: ActionTemplate
vault player = gameState' >>= inner
  where
    inner state =
      (chooseMany (EffectDiscard unknown (Hand player)) h (0,length h) (\cards -> seqActions (\c -> discard c (Hand player)) cards &&& plusMoney (length cards))
       &&& eachOtherPlayer otherAction)
        player
      where
        h = hand $ playerByName state player
        otherCont player cards = (seqActions (\c -> discard c (Hand player)) cards &&& if length cards == 2 then plusCards 1 else pass) player
        otherAction player = gameState' >>= i2
          where
            i2 state
              | null h2 = noOpSimulation
              | otherwise = optDecision (ChooseCards (EffectDiscard unknown (Hand player)) h2 (num,num) (otherCont player)) player
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
   action 606 "Remake" 4 (trashForGain (gainUpgrade 1) &&& trashForGain (gainUpgrade 1)),
   withTrigger (action 607 "Tournament" 4 (plusActions 1 &&& tournament))
    (onStartOfGame (seqActions addNonSupplyPile prizes)),
   notImplemented "Young Witch", -- 608 young witch
   action 609 "Harvest" 5 harvest,
   carddefA 610 "Horn of Plenty" (simpleCost 5) [Treasure] noPoints hornOfPlenty noTriggers,
   action 611 "Hunting Party" 5 (plusCards 1 &&& plusActions 1 &&& huntingParty),
   attack 612 "Jester" 5 (plusMoney 2 &&& jester),
   withInitialSupply (carddef 613 "Fairgrounds" (simpleCost 6) [Victory] fairgroundsPoints pass noTriggers)
    stdVictorySupply
  ]

prizes = map (($ Cornucopia) . (\gen -> withInitialSupply gen (const 1)))
  [carddef 620 "Bag of Gold" (simpleCost 0) [Action, Prize] noPoints (\p -> (plusActions 1 &&& gainTo gold (TopOfDeck p)) p) noTriggers,
   carddef 621 "Diadem" (simpleCost 0) [Treasure, Prize] noPoints (plusMoney 2 &&& (\p -> gameState' >>= \s -> plusMoney (actions (turn s)) p)) noTriggers,
   carddef 622 "Followers" (simpleCost 0) [Action, Attack, Prize] noPoints
    (plusCards 2 &&& gain estate &&& playAttack (gain curse &&& discardDownTo 3))
    noTriggers,
   carddefA 623 "Princess" (simpleCost 0) [Action, Prize] noPoints princess noTriggers,
   carddef 624 "Trusty Steed" (simpleCost 0) [Action, Prize] noPoints trustySteed noTriggers
   ]

princess :: Maybe Card -> ActionTemplate
princess Nothing = plusBuys 1
princess (Just card) = plusBuys 1 &&& addModifier (ModCost Nothing) (ConditionalModifier ((card `elem`) . inPlay . activePlayer) (CappedDecModifier 2))

fairgroundsPoints :: Player -> Int
fairgroundsPoints p = 2 * ((length $ L.nub $ map typ $ allCards p) `quot` 5)

farmingVillage :: ActionTemplate
farmingVillage = revealUntilSelector (\c -> isAction c || isTreasure c)
                                     (\match other p -> (discardAll other (TopOfDeck p) &&& putAll match (TopOfDeck p) (Hand p)) p)

fortuneTeller :: ActionTemplate
fortuneTeller = revealUntilSelector (\c -> typ c == curse || isVictory c) (\_ other p -> discardAll other (TopOfDeck p) p)

hamlet :: ActionTemplate
hamlet = (\p -> gameState' >>= \s ->
                  optDecision
                    (ChooseCard (EffectDiscard unknown (Hand p)) (hand (playerByName s p)) (\c -> (discard c (Hand p) &&& plusActions 1) p))
                    p)
  &&& (\p -> gameState' >>= \s ->
               optDecision
                (ChooseCard (EffectDiscard unknown (Hand p)) (hand (playerByName s p)) (\c -> (discard c (Hand p) &&& plusBuys 1) p))
                p)

harvest :: ActionTemplate
harvest = revealUntil ((==4) . length) (\cs p -> (discardAll cs (TopOfDeck p) &&& plusMoney (length cs)) p)

hornOfPlenty :: Maybe Card -> ActionTemplate
hornOfPlenty source p = gameState' >>= inner
  where
    inner s
      | null candidates = noOpSimulation
      | otherwise = chooseOne
                      (EffectGain unknownDef (Discard p))
                      (map (`topOfSupply` s) candidates)
                      (\c -> gain (typ c) &&& (if isVictory c then trashHorn source else pass))
                      p
      where
        candidates = affordableCards (simpleCost typesInPlay) s
        typesInPlay = length $ L.nub $ map typ (inPlay player ++ Either.lefts (inPlayDuration player))
        player = playerByName s p
        trashHorn Nothing = pass
        trashHorn (Just card) = trash card InPlay

huntingParty :: ActionTemplate
huntingParty p = gameState' >>= inner
  where
    inner s =
      (reveal h &&&
       revealUntilSelector (\c -> not ((typ c) `elem` htypes))
                           (\match other p -> (discardAll other (TopOfDeck p) &&& putAll match (TopOfDeck p) (Hand p)) p))
       p
      where
        h = hand $ playerByName s p
        htypes = L.nub $ map typ $ h

jester :: ActionTemplate
jester attacker = playAttack (reshuffleIfNeeded &&& (\op -> gameState' >>= \state -> attack op state)) attacker
  where
    attack op state
      | null d = noOpSimulation
      | isVictory top = gain curse op
      | otherwise = chooseEffects 1 [EffectGain (typ top) (Discard op), EffectGain (typ top) (Discard attacker)]
                      enact
                      attacker
      where
        d = deck (playerByName state op)
        top = head d
        enact [(EffectGain _ (Discard p))] _ = gain (typ top) p
        enact _ _ = error "Unexpected choice from Jester"

menagerie :: ActionTemplate
menagerie player = gameState' >>= inner
  where
    inner state = (reveal h &&& (if unique then plusCards 3 else plusCards 1)) player
      where
        h = hand $ playerByName state player
        cardTypes = map typ h
        unique = length cardTypes == length (L.nub cardTypes)

tournament :: ActionTemplate
tournament playerId = gameState' >>= inner
  where
    inner s = allDecisions Map.empty (playerNames s)
      where
        final :: Map.Map PlayerId Bool -> SimulationT ()
        final choices =
          (seqActions (\p _ -> reveal [(head (provinces p))] p) (Map.keys $ Map.filter id choices)
           &&& (if Map.findWithDefault False playerId choices then discard (head (provinces playerId)) (Hand playerId) &&& gainPrize else pass)
           &&& (if Map.size (Map.filterWithKey (\k v -> v && k /= playerId) choices) > 0 then pass else plusCards 1 &&& plusMoney 1))
          playerId

        provinces player = filter ((==province) . typ) $ hand $ playerByName s player

        gainPrize :: ActionTemplate
        gainPrize player = chooseEffects 1 [SpecialEffect cTournament, EffectGain duchy (TopOfDeck player)] (enact . head) player

        enact :: Effect -> ActionTemplate
        enact (SpecialEffect _) p
          | null ps = noOpSimulation
          | otherwise = chooseOne (EffectGain unknownDef (TopOfDeck p)) ps (\c -> gainSpecial (typ c) (TopOfDeck p)) p
          where
            ps = map head $ Map.elems $ Map.filter (not . null) (nonSupplyPiles s)
        enact e p = enactEffect e p

        allDecisions :: Map.Map PlayerId Bool -> [PlayerId] -> SimulationT ()
        allDecisions accu [] = final accu
        allDecisions accu (p:ps)
          | null (provinces p) = allDecisions (Map.insert p False accu) ps
          | otherwise = decision (ChooseToUse (EffectReveal (head (provinces p))) (\b -> allDecisions (Map.insert p b accu) ps)) p

trustySteed :: ActionTemplate
trustySteed p = chooseEffects 2 [EffectPlusCards 2, EffectPlusActions 2, EffectPlusMoney 2, SpecialEffect cTrustySteed] enactAll p
  where
    enact (SpecialEffect _) = gain silver &&& gain silver &&& gain silver &&& gain silver &&& deckIntoDiscard
    enact e = enactEffect e
    enactAll = foldr (\e a -> enact e &&& a) pass


-- Hinterlands 7xx

hinterlandCards = map ($ Hinterlands)
  [action 701 "Crossroads" 2 crossRoads,
   withTrigger (action 702 "Duchess" 2 (plusMoney 2 &&& eachPlayer (reshuffleIfNeeded &&& duchess))) duchessTrigger,
   notImplemented "Fool's Gold", -- 703 fool's gold
   -- TODO ordering ...
   action 704 "Develop" 3 (trashForGain (\c p -> (gainUpgradeTo 1 (TopOfDeck p) c &&& gainUpgradeTo (-1) (TopOfDeck p) c) p)),
   action 705 "Oasis" 3 (plusCards 1 &&& plusActions 1 &&& plusMoney 1 &&& discardNCards 1),
   notImplemented "Oracle", -- 706 oracle
   notImplemented "Scheme", -- 707 scheme
   carddef 708 "Tunnel" (simpleCost 3) [Victory, Reaction] (const 2)
    pass
    (onDiscardSelf tunnelTrigger),
   action 709 "Jack of All Trades" 4 jackOfAllTrades,
   notImplemented "Noble Brigand", -- 710 noble brigand
   withTrigger (action 711 "Nomad Camp" 4 (plusBuys 1 &&& plusMoney 2)) nomadCamp,
   withInitialSupply
    (carddef 712 "Silk Road" (simpleCost 4) [Victory] (\p -> length (filter isVictory (allCards p)) `quot` 4) pass noTriggers)
    stdVictorySupply,
   action 713 "Spice Merchant" 4 spiceMerchant,
   carddef 714 "Trader" (simpleCost 4) [Action, Reaction] noPoints
    (trashForGain (\c p -> gameState' >>= \s -> seqActions gain (replicate (moneyCost (cost s (typ c))) silver) p))
    (whileInHand traderTrigger),
   carddef 715 "Cache" (simpleCost 5) [Treasure] noPoints (plusMoney 3) (onGainSelf (gain copper &&& gain copper)),
   notImplemented "Cartographer", -- 716 cartographer
   withTrigger (action 717 "Embassy" 5 (plusCards 5 &&& discardNCards 3))
    (onGainSelf (eachOtherPlayer (gain silver))),
   withTrigger (action 718 "Haggler" 5 (plusMoney 2))
    (whileInPlay (onBuy haggler)),
   actionA 719 "Highway" 5 highway,
   carddef 720 "Ill-gotten Gains" (simpleCost 5) [Treasure] noPoints
    (\p -> (plusMoney 1 &&& choose (ChooseToUse (EffectGain copper (Hand p))) (\b -> if b then gainTo copper (Hand p) else pass)) p)
    (onGainSelf (eachOtherPlayer (gain curse))),
   notImplemented "Inn", -- 721 inn
   withTrigger (action 722 "Mandarin" 5 (plusMoney 3 &&& mandarinAction)) (onGainSelf mandarinTrigger),
   attack 723 "Margrave" 5 (plusCards 3 &&& plusBuys 1 &&& playAttack (plusCards 1 &&& discardDownTo 3)),
   action 724 "Stables" 5 stables,
   withTrigger (action 725 "Border Village" 6 (plusCards 1 &&& plusActions 2)) (onGainSelf borderVillageTrigger),
   withTrigger (withInitialSupply (victory 726 "Farmland" 6 2) stdVictorySupply)
    (onBuySelf (trashForGain (gainUpgrade 2)))
   ]

borderVillageTrigger :: ActionTemplate
borderVillageTrigger p = gameState' >>= \s -> gainUpto ((moneyCost (cost s cBorderVillage)) - 1) p

crossRoads :: ActionTemplate
crossRoads p = gameState' >>= inner
  where
    inner s = (reveal h
                  &&& (if numVictory > 0 then plusCards numVictory else pass)
                  &&& (if numCrossroads == 1 then plusActions 3 else pass))
                  p
      where
        h = hand $ playerByName s p
        numVictory = length $ filter isVictory h
        numCrossroads = length $ filter (==cCrossroads) $ playsThisTurn s

duchess :: ActionTemplate
duchess p = gameState' >>= inner
  where
    inner s
      | null d = noOpSimulation
      | otherwise = choose (ChooseToUse (EffectDiscard top (TopOfDeck p))) (\b -> if b then discard top (TopOfDeck p) else pass) p
      where
        d = deck $ playerByName s p
        top = head d

optGainDuchess :: ActionTemplate
optGainDuchess p = choose (ChooseToUse (EffectGain cDuchess (Discard p))) (\b -> if b then gain cDuchess else pass) p

duchessTrigger :: TriggerHandler
duchessTrigger GainTrigger (EffectGainFrom c1 _ _) (FromSupply _) cont = if duchy == (typ c1) then optGainDuchess &&& cont else cont
duchessTrigger GainTrigger (EffectGain d1 _) (FromSupply _) cont = if duchy == d1 then optGainDuchess &&& cont else cont
duchessTrigger _ _ _ cont = cont

jackOfAllTrades :: ActionTemplate
jackOfAllTrades = gain silver &&& spyTop &&& drawTo5 &&& optTrash
  where
    spyTop player = do
      reshuffleIfNeeded player
      state <- gameState'
      if null (deck (playerByName state player)) then noOpSimulation else chooseKeepDiscard player

    chooseKeepDiscard player = do
      state <- gameState'
      let top = head $ deck $ playerByName state player
          cont b = if b then discard top (TopOfDeck player) player else noOpSimulation
      (addLog (LogPeek (VisibleToPlayer player) player [top] (TopOfDeck player))
       &&& decision (ChooseToUse (EffectDiscard top (TopOfDeck player)) cont))
       player

    drawTo5 player = do
      state <- gameState'
      let num = length $ hand $ playerByName state player
      if num >= 5 then noOpSimulation else plusCards (5-num) player

    optTrash player = gameState' >>= inner
      where
        inner state
          | null candidates = noOpSimulation
          | otherwise = optDecision (ChooseCard (EffectTrash unknown (Hand player)) candidates cont) player
          where
            candidates = filter (not . isTreasure) $ hand $ playerByName state player
            cont card = trash card (Hand player) player

haggler :: CardDef -> ActionTemplate
haggler card p = gameState' >>= inner
  where
    inner s
      | null candidates = noOpSimulation
      | otherwise = chooseOne (EffectGain unknownDef (Discard p)) (map (`topOfSupply` s) candidates) (gain . typ) p
      where
        candidates = filter (not . (`hasType` Victory)) $ affordableCards (subtractCost (cost s card) (simpleCost 1)) s

highway :: Maybe Card -> ActionTemplate
highway Nothing = plusCards 1 &&& plusActions 1
highway (Just card) = plusCards 1 &&& plusActions 1 &&&
  addModifier (ModCost Nothing) (ConditionalModifier ((card `elem`) . inPlay . activePlayer) (CappedDecModifier 1))

mandarinAction :: ActionTemplate
mandarinAction player = gameState' >>= inner
  where
    inner state
      | null h = noOpSimulation
      | otherwise = chooseOne (EffectPut unknown (Hand player) (TopOfDeck player)) h (\c -> put c (Hand player) (TopOfDeck player)) player
      where
        h = hand $ playerByName state player

mandarinTrigger :: ActionTemplate
mandarinTrigger player = do
  state <- gameState'
  let treasures = filter isTreasure $ inPlay (playerByName state player)
  seqActions (\c -> put c InPlay (TopOfDeck player)) treasures player

nomadCamp :: TriggerHandler
nomadCamp GainTrigger (EffectGainFrom c1 source target) (FromCard c2 _) cont
  | c1 == c2 =
    case target of
      (TopOfDeck _) -> cont
      _ -> \p -> gainFromTo c1 source (TopOfDeck p) p
  | otherwise = cont
nomadCamp GainTrigger (EffectGain d1 target) (FromCardEffect d2) cont
  | d1 == d2 =
    case target of
      (TopOfDeck _) -> cont
      _ -> \p -> gainTo d1 (TopOfDeck p) p
  | otherwise = cont
nomadCamp _ _ _ cont = cont

spiceMerchant :: ActionTemplate
spiceMerchant p = gameState' >>= inner
  where
    inner s
      | null ts = noOpSimulation
      | otherwise = optDecision (ChooseCard (EffectDiscard unknown (Hand p)) ts (\card -> (trash card (Hand p) &&& benefit) p)) p
      where
        ts = filter isTreasure $ hand $ playerByName s p
        benefit = chooseEffects 1 [MultiEffect [EffectPlusCards 2, EffectPlusActions 1], MultiEffect [EffectPlusMoney 2, EffectPlusBuys 1]] enactEffects

stables :: ActionTemplate
stables p = gameState' >>= inner
  where
    inner s
      | null ts = noOpSimulation
      | otherwise = optDecision (ChooseCard (EffectDiscard unknown (Hand p)) ts (\card -> (discard card (Hand p) &&& plusCards 3 &&& plusActions 1) p)) p
      where
        ts = filter isTreasure $ hand $ playerByName s p

traderTrigger :: TriggerHandler
traderTrigger _ (EffectGainFrom c _ _) (FromCard trader (Hand _)) cont
  | typ c == silver = cont
  | otherwise = choose (ChooseToUse (EffectReveal trader)) (\b -> if b then reveal [trader] &&& gain silver else cont)
traderTrigger _ (EffectGain c _) (FromCard trader (Hand _)) cont
  | c == silver = cont
  | otherwise = choose (ChooseToUse (EffectReveal trader)) (\b -> if b then reveal [trader] &&& gain silver else cont)
traderTrigger _ _ _ cont = cont

tunnelTrigger :: Card -> ActionTemplate
tunnelTrigger c p = gameState' >>= inner
  where
    inner s
      | phase (turn s) == CleanupPhase = noOpSimulation
      | otherwise = choose (ChooseToReact c DiscardTrigger) (\b -> if b then reveal [c] &&& gain gold else pass) p

-- Dark Ages 8xx

darkAgesCards = map ($ DarkAges)
  [action 801 "Poor House" 1 poorHouse,
   carddef 802 "Beggar" (simpleCost 2) [Action, Reaction] noPoints
    (\p -> (gainTo copper (Hand p) &&& gainTo copper (Hand p) &&& gainTo copper (Hand p)) p)
    (whileInHand (onAttackA beggarTrigger)),
   withTrigger
    (action 803 "Squire" 2
      (\p -> (plusMoney 1 &&& chooseEffects 1 [EffectPlusActions 2, EffectPlusBuys 2, EffectGain silver (Discard p)] enactEffects) p))
    (onTrashSelf squireTrigger),
   action 804 "Vagrant" 2 (plusCards 1 &&& plusActions 1 &&& reshuffleIfNeeded &&& vagrant),
   action 805 "Forager" 3 (plusActions 1 &&& plusBuys 1 &&& trashForGain (\_ p -> gameState' >>= \s -> plusMoney (length $ L.nub $ map typ $ filter isTreasure $ trashPile s) p)),
   notImplemented "Hermit", -- 806 Hermit
   carddef 807 "Market Square" (simpleCost 3) [Action, Reaction] noPoints (plusCards 1 &&& plusActions 1 &&& plusBuys 1)
    (whileInHand (onTrash marketSquareTrigger)),
   action 808 "Sage" 3 (plusActions 1 &&& sage),
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
   carddef 816 "Marauder" (simpleCost 4) [Action, Attack, Looter] noPoints (\p -> (gainSpecial cSpoils (Discard p) &&& playAttack (gain ruinsPseudoDef)) p)
    (onStartOfGame (addNonSupplyPile cSpoils)),
   action 817 "Procession" 4 procession,
   notImplemented "Rats", -- 818 Rats
   notImplemented "Scavenger", -- 819 Scavenger
   notImplemented "Wandering Minstrel", -- 820 Wandering Minstrel
   notImplemented "Band of Misfits", -- 821 Band of Misfits
   withTrigger (action 822 "Bandit Camp" 5 (plusCards 1 &&& plusActions 2 &&& (\p -> gainSpecial cSpoils (Discard p) p)))
    (onStartOfGame (addNonSupplyPile cSpoils)),
   notImplemented "Catacombs", -- 823 Catacombs
   action 824 "Count" 5 count,
   carddef 825 "Counterfeit" (simpleCost 5) [Treasure] noPoints (plusMoney 1 &&& plusBuys 1 &&& counterfeit) noTriggers,
   carddef 826 "Cultist" (simpleCost 5) [Action, Attack, Looter] noPoints (plusCards 2 &&& playAttack (gain ruinsPseudoDef) &&& cultistCascade)
    (onTrashSelf (plusCards 3)),
   notImplemented "Graverobber", -- 827 Graverobber
   action 828 "Junk Dealer" 5 (plusCards 1 &&& plusActions 1 &&& plusMoney 1 &&& trashNCards 1 1),
   notImplemented "Knights", -- 829 Knights
   -- TODO refactor wishing well into a better action
   action 830 "Mystic" 5 (plusActions 1 &&& plusMoney 2 &&& reshuffleIfNeeded &&& nameACard (SpecialEffect cMystic) wishingWell),
   notImplemented "Pillage", -- 831 Pillage
   action 832 "Rebuild" 5 (plusActions 1 &&& rebuild),
   notImplemented "Rogue", -- 833 Rogue
   action 834 "Altar" 6 (trashForGain (\_ -> gainUpto 5)),
   withTrigger (action 835 "Hunting Grounds" 6 (plusCards 4))
    (onTrashSelf (\p -> chooseEffects 1
                            [EffectGain duchy (Discard p), MultiEffect (replicate 3 (EffectGain estate (Discard p)))]
                            enactEffects p))
  ]

darkAgesExtra = map ($ DarkAges)
  [carddefA 860 "Madman" (simpleCost 0) [Action] noPoints madman noTriggers,
   withInitialSupply (carddefA 861 "Spoils" (simpleCost 0) [Treasure] noPoints spoils noTriggers) (const 15)]

madman :: Maybe Card -> ActionTemplate
madman Nothing = plusActions 2
madman (Just card) = plusActions 2 &&& put card InPlay NonSupply &&& (\p -> gameState' >>= \s -> plusCards (length (hand (playerByName s p))) p)

marketSquareTrigger :: ActionTemplate
marketSquareTrigger p = do
  s <- gameState'
  let msq = head $ filter ((==cMarketSquare) . typ) $ hand $ playerByName s p
  choose (ChooseToReact msq TrashTrigger) (\b -> if b then discard msq (Hand p) &&& gain gold else pass) p

spoils :: Maybe Card -> ActionTemplate
spoils Nothing = plusMoney 3
spoils (Just card) = plusMoney 3 &&& put card InPlay NonSupply

armoryGain :: ActionTemplate
armoryGain player = gameState' >>= inner
  where
    inner state = decision
        (ChooseCard (EffectGain unknownDef (TopOfDeck player))
                    (map (`topOfSupply` state) candidates)
                    (\c -> gainTo (typ c) (TopOfDeck player) player))
        player
      where
        candidates = affordableCards (simpleCost 4) state

beggarTrigger :: TriggerSource -> ActionTemplate
beggarTrigger (FromCard card _) p = choose (ChooseToUse (EffectDiscard card (Hand p)))
                                       (\b -> if b then discard card (Hand p) &&& gainTo silver (TopOfDeck p) &&& gainTo silver (TopOfDeck p)
                                                   else pass)
                                       p
beggarTrigger _ p = pass p

count :: ActionTemplate
count player =
  (chooseEffects 1 [EffectDiscardNo 2, EffectPut unknown (Hand player) (TopOfDeck player), EffectGain copper (Discard player)] enactEffects
   &&& (\p -> gameState' >>= \s -> chooseEffects 1 [EffectPlusMoney 3, EffectTrashNo (length (hand (playerByName s p))), EffectGain duchy (Discard player)]
                                    enactEffects p))
    player

counterfeit :: ActionTemplate
counterfeit p = gameState' >>= inner
  where
    inner s
      | null ts = noOpSimulation
      | otherwise = choose (ChooseToUse (SpecialEffect cCounterfeit))
                           (\b -> if b then cont else pass)
                           p
      where
        ts = filter isTreasure $ hand $ playerByName s p
        cont = chooseOne (EffectPlayTreasure unknown) ts (\card -> play card &&& playEffect (typ card) Nothing &&& trash card InPlay)

cultistCascade :: ActionTemplate
cultistCascade p = gameState' >>= inner
  where
    inner s
      | null cultists = noOpSimulation
      | otherwise = choose (ChooseToUse (EffectPlayAction (head cultists)))
                           (\b -> if b then play (head cultists) else pass)
                           p
      where
        cultists = filter ((==cCultist) . typ) $ hand $ playerByName s p


fortressTrigger :: TriggerHandler
fortressTrigger TrashTrigger (EffectTrash c1 _) (FromCard c2 _) cont player =
  if c1 == c2 then (cont &&& put c1 Trash (Hand player)) player
              else cont player
fortressTrigger _ _ _ cont p = cont p

ironmonger :: ActionTemplate
ironmonger player = gameState' >>= inner
  where
    inner state
      | null top = noOpSimulation
      | otherwise =
        (reveal [t]
         &&& decision (ChooseToUse (EffectDiscard t (TopOfDeck player)) (\b -> if b then discard t (TopOfDeck player) player
                                                                                    else noOpSimulation))
         &&& (if isAction t then plusActions 1 else pass)
         &&& (if isTreasure t then plusMoney 1 else pass)
         &&& (if isVictory t then plusCards 1 else pass))
        player
      where
        top = getCards (TopOfDeck player) state
        t = head top

poorHouse :: ActionTemplate
poorHouse = plusMoney 4 &&& \p ->
  gameState' >>= \s ->
  plusMoney (- (min (money (turn s))
                    (length (filter isTreasure (hand (playerByName s p))))))
    p

procession :: ActionTemplate
procession p = gameState' >>= inner
  where
    inner s
      | null actions = noOpSimulation
      | otherwise = optDecision
                      (ChooseCard (SpecialEffect cProcession)
                                  actions
                                  (\card -> (play card
                                             &&& playEffect (typ card) Nothing
                                             &&& trash card InPlay
                                             &&& gainUpgrade 1 card)
                                             p))
                      p
      where
        actions = filter isAction $ hand $ playerByName s p

rebuild :: ActionTemplate
rebuild = nameACard (SpecialEffect cRebuild)
                    (\card -> revealUntilSelector
                                (\c -> isVictory c && not (typ c == card))
                                (\matches others p -> (discardAll others (TopOfDeck p)
                                                         &&& trashAll matches (TopOfDeck p)
                                                         &&& gainUpgrade matches)
                                                         p))
  where
    gainUpgrade [c] p = gameState' >>= \s -> gainUptoFiltered (3 + moneyCost (cost s (typ c))) (`hasType` Victory) p
    gainUpgrade _ _ = noOpSimulation


sage :: ActionTemplate
sage p = gameState' >>= \s -> revealUntilSelector (\card -> moneyCost (cost s (typ card)) >= 3)
                                                  (\match other -> putAll match (TopOfDeck p) (Hand p) &&& discardAll other (TopOfDeck p))
                                                  p

squireTrigger :: ActionTemplate
squireTrigger p = gameState' >>= inner
  where
    inner s
      | null attacks = noOpSimulation
      | otherwise = chooseOne (EffectGain unknownDef (Discard p)) attacks (\c -> gain (typ c)) p
      where
        attacks = filter isAttack $ map (head . snd) $ Map.toList $ Map.filter (not . null) $ piles s

vagrant :: ActionTemplate
vagrant p = gameState' >>= inner
  where
    inner s
      | null d = noOpSimulation
      | isCurse top && isRuins top && isShelter top && isVictory top = (reveal [top] &&& put top (TopOfDeck p) (Hand p)) p
      | otherwise = reveal [top] p
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
    (onStartOfGame (\_ -> gameState' >>= \state -> M.mapM_ (plusTokens 1 CoinToken . name) (Map.elems $ players state))),
  notImplemented "Butcher", -- 910
  notImplemented "Journeyman", -- 911
  withTrigger (action 912 "Merchant Guild" 5 (plusBuys 1 &&& plusMoney 1))
    (whileInPlay (onBuy (\_ -> plusTokens 1 CoinToken))),
  notImplemented "Soothsayer" -- 913
  ]

plaza :: ActionTemplate
plaza p = gameState' >>= inner
  where
    inner s
      | null ts = noOpSimulation
      | otherwise = optDecision (ChooseCard (EffectDiscard unknown (Hand p)) ts (\c -> discard c (Hand p) p)) p
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

amulet :: ActionTemplate
amulet p = chooseEffects 1 [EffectPlusMoney 1, EffectTrashNo 1, EffectGain silver (Discard p)] enactEffects p

-- Promo 20xx

promoCards = map ($ Promo) []