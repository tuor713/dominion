{-# LANGUAGE TupleSections #-}
module Dominion.Model where

import qualified Control.Monad as M
import qualified Control.Monad.Trans.State.Lazy as St
import qualified Data.Either as Either
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import System.Random (StdGen, randomR)

import Data.Array.ST
import Control.Monad
import Control.Monad.ST
import Data.STRef

data CardType = Action | Treasure | Victory | CurseType {- This is a bit of a hack -} |
    Reaction | Attack | Duration |
    Prize | Looter | Ruins | Knight | Reserve | Traveller
    deriving (Eq, Ord, Read, Show)

data Location = Hand PlayerId | Discard PlayerId | TopOfDeck PlayerId |
  InPlay | InPlayDuration | Trash | Supply | Mat PlayerId Mat
  deriving (Eq, Show)

data Edition = Base | Intrigue | Seaside | Alchemy | Prosperity | Cornucopia | Hinterlands | DarkAges | Guilds | Adventures | Promo
  deriving (Eq, Read, Show)


-- The fundamental domain types

-- Could have chosen to implement this with complex numbers ;)
newtype Cost = Cost (Int,Int)

moneyCost :: Cost -> Int
moneyCost (Cost (m,_)) = m

potionCost :: Cost -> Int
potionCost (Cost (_,p)) = p

simpleCost :: Int -> Cost
simpleCost m = Cost (m,0)

fullCost :: Int -> Int -> Cost
fullCost money potions = Cost (money,potions)

instance Eq Cost where
  (==) (Cost (m1,p1)) (Cost (m2,p2)) = m1 == m2 && p1 == p2

-- Note this is a partial order, hence we do not implement Ord
smallerEqCost :: Cost -> Cost -> Bool
smallerEqCost (Cost (m1,p1)) (Cost (m2,p2)) = m1 <= m2 && p1 <= p2

addCost :: Cost -> Cost -> Cost
addCost (Cost (m1,p1)) (Cost (m2,p2)) = Cost (m1+m2,p1+p2)

subtractCost :: Cost -> Cost -> Cost
subtractCost (Cost (m1,p1)) (Cost (m2,p2)) = Cost (m1-m2,p1-p2)

data CardDef = CardDef { -- Card id is purely a performance artifact to avoid string comparisons
                   cardTypeId :: !Int,
                   cardName :: !String,
                   edition :: !Edition,
                   cardCost :: Cost,
                   types :: ![CardType],
                   cardPoints :: !(Player -> Int),
                   onPlay :: !(Maybe Card -> Action),
                   initialSupply :: Int -> Int,
                   triggers :: Map.Map Trigger Action,
                   canBuy :: GameState -> Bool
                   }

data Card = Card { cardId :: !Int, typ :: !CardDef }

type CardLike = Either Card CardDef

instance Show CardDef where
  show = cardName

instance Eq CardDef where
  c1 == c2 = cardTypeId c1 == cardTypeId c2

instance Ord CardDef where
  c1 <= c2 = cardTypeId c1 <= cardTypeId c2

instance Show Card where
  show card = show (typ card) ++ " (" ++ show (cardId card) ++ ")"

instance Eq Card where
  c1 == c2 = cardId c1 == cardId c2

instance Ord Card where
  c1 <= c2 = cardId c1 <= cardId c2


-- Card definition helpers

noPoints :: a -> Int
noPoints = const 0

noTriggers :: Map.Map Trigger Action
noTriggers = Map.empty

-- TODO refactor these to use carddef* instead
treasure :: Int -> String -> Int -> Int -> Edition -> CardDef
treasure id name cost money edition = CardDef id name edition (simpleCost cost) [Treasure] noPoints (\_ -> plusMoney money) (const 10) noTriggers (const True)

action :: Int -> String -> Int -> Action -> Edition -> CardDef
action id name cost effect edition = CardDef id name edition (simpleCost cost) [Action] noPoints (\_ -> effect) (const 10) noTriggers (const True)

actionA :: Int -> String -> Int -> (Maybe Card -> Action) -> Edition -> CardDef
actionA id name cost effect edition = CardDef id name edition (simpleCost cost) [Action] noPoints effect (const 10) noTriggers (const True)

duration :: Int -> String -> Int -> Action -> Action -> Edition -> CardDef
duration id name cost effect startOfTurn edition =
  CardDef id name edition (simpleCost cost) [Action, Duration] noPoints  (\_ -> effect) (const 10) (Map.singleton StartOfTurnTrigger startOfTurn) (const True)

attack :: Int -> String -> Int -> Action -> Edition -> CardDef
attack id name cost effect edition = CardDef id name edition (simpleCost cost) [Action, Attack] noPoints (\_ -> effect) (const 10) noTriggers (const True)

victory :: Int -> String -> Int -> Int -> Edition -> CardDef
victory id name cost points edition = CardDef id name edition (simpleCost cost) [Victory] (const points) (\_ -> pass) (const 10) noTriggers (const True)

carddef :: Int -> String -> Cost -> [CardType] -> (Player -> Int) -> Action -> Map.Map Trigger Action -> Edition -> CardDef
carddef id name cost types points effect triggers edition = CardDef id name edition cost types points (\_ -> effect) (const 10) triggers (const True)

carddefA :: Int -> String -> Cost -> [CardType] -> (Player -> Int) -> (Maybe Card -> Action) -> Map.Map Trigger Action -> Edition -> CardDef
carddefA id name cost types points effect triggers edition = CardDef id name edition cost types points effect (const 10) triggers (const True)


withTrigger :: (Edition -> CardDef) -> Trigger -> Action -> Edition -> CardDef
withTrigger cardgen trigger action ed = card { triggers = Map.insert trigger action (triggers card) }
  where
    card = cardgen ed

withInitialSupply :: (Edition -> CardDef) -> (Int -> Int) -> Edition -> CardDef
withInitialSupply cardgen supplyf ed = (cardgen ed) { initialSupply = supplyf }

withBuyRestriction :: (Edition -> CardDef) -> (GameState -> Bool) -> Edition -> CardDef
withBuyRestriction cardgen pred ed = (cardgen ed) { canBuy = pred }

-- Basic cards

stdVictorySupply :: Int -> Int
stdVictorySupply 2 = 8
stdVictorySupply _ = 12

copper   = withInitialSupply (treasure 0 "Copper" 0 1) (\players -> 60 - 7 * players) Base
silver   = withInitialSupply (treasure 1 "Silver" 3 2) (const 40) Base
gold     = withInitialSupply (treasure 2 "Gold" 6 3) (const 30) Base
estate   = withInitialSupply (victory 3 "Estate" 2 1) stdVictorySupply Base
duchy    = withInitialSupply (victory 4 "Duchy" 5 3) stdVictorySupply Base
province = withInitialSupply (victory 5 "Province" 8 6) stdVictorySupply Base
curse    = withInitialSupply (carddef 6 "Curse" (simpleCost 0) [CurseType] (const (-1)) pass noTriggers)
            (\players -> if players == 2 then 10 else if players == 3 then 20 else 30)
            Base
potion   = withInitialSupply (carddef 7 "Potion" (simpleCost 4) [Treasure] (const 0) plusPotion noTriggers) (const 16) Alchemy
platinum = withInitialSupply (treasure 8 "Platinum" 9 5) (const 12) Prosperity
colony   = withInitialSupply (victory 9 "Colony" 11 10) stdVictorySupply Prosperity

basicCards = [copper, silver, gold, estate, duchy, province, curse, potion, platinum, colony]



type PlayerId = String

data Mat = IslandMat deriving (Eq, Ord, Show)
data Token = VictoryToken deriving (Eq, Ord, Show)

data Player = Player { name :: PlayerId,
                       hand :: [Card],
                       inPlay :: [Card],
                       inPlayDuration :: [CardLike],
                       deck :: [Card],
                       discardPile :: [Card],
                       mats :: Map.Map Mat [Card],
                       tokens :: Map.Map Token Int
                       }
                       deriving (Eq, Show)

data ModAttribute = ModCost (Maybe CardType) deriving (Eq, Ord, Show)
data Modifier = NullModifier | CappedDecModifier Int | StackedModifier [Modifier] |
  ConditionalModifier (GameState -> Bool) Modifier

instance Show Modifier where
  show NullModifier = "NullModifier"
  show (CappedDecModifier num) = "CappedDecModifier(" ++ show num ++ ")"
  show (ConditionalModifier _ mod) = "ConditionalModifier(" ++ show mod ++ ")"
  show (StackedModifier mods) = "Modifiers(" ++ (L.intercalate ", " $ map show mods) ++ ")"

applyMod :: Modifier -> GameState -> Int -> Int
applyMod NullModifier _ x = x
applyMod (CappedDecModifier num) _ x = max 0 (x - num)
applyMod (StackedModifier mods) state x = foldr (\m res -> applyMod m state res) x mods
applyMod (ConditionalModifier pred mod) state x = if pred state then applyMod mod state x else x

stackMod :: Modifier -> Modifier -> Modifier
stackMod mod NullModifier = mod
stackMod NullModifier mod = mod
stackMod (StackedModifier xs) (StackedModifier ys) = StackedModifier (xs ++ ys)
stackMod mod (StackedModifier xs) = StackedModifier (mod:xs)
stackMod (StackedModifier xs) mod = StackedModifier (xs ++ [mod])
stackMod m1 m2 = StackedModifier [m1,m2]

data TurnState = TurnState { money :: Int,
                             potions :: Int,
                             buys :: Int,
                             actions :: Int,
                             modifiers :: Map.Map ModAttribute Modifier
                             }
                             deriving (Show)


data GameType = StandardGame | ColonyGame deriving (Eq, Read, Show)

data GameState = GameState { players :: Map.Map PlayerId Player,
                             turnOrder :: [PlayerId],
                             trashPile :: [Card],
                             turn :: TurnState,
                             piles :: Map.Map CardDef [Card],
                             ply :: Int,
                             finished :: Bool
                             }

data Result = Tie [PlayerId] | Win PlayerId deriving (Eq, Read, Show)

data Visibility = AllPlayers | VisibleToPlayer PlayerId deriving (Eq)

instance Show Visibility where
  show AllPlayers = "All"
  show (VisibleToPlayer player) = player

type Info = (Visibility, String)
type SimulationState = (StdGen, [Info])

type SimulationT a = St.State SimulationState a
type Simulation = SimulationT GameStep

-- Simulation primitives

randomGen :: SimulationT StdGen
randomGen = M.liftM fst St.get

setRandomGen :: StdGen -> SimulationT ()
setRandomGen gen =
  do
    (_,infos) <- St.get
    St.put (gen,infos)
    return ()

info :: Visibility -> String -> SimulationT ()
info vis msg = St.get >>= \(gen,infos) -> St.put (gen,(vis,msg):infos) >> return ()

canSee :: PlayerId -> Info -> Bool
canSee _ (AllPlayers,_) = True
canSee pid (VisibleToPlayer p,_) = pid == p

-- TODO add source card
data Trigger =
  AttackTrigger |
  BuyTrigger |
  TrashTrigger |
  StartOfTurnTrigger
  deriving (Eq, Ord, Show)

-- Numeric generic effect and placeholder effects
-- are not the same
-- 'trash 2' is not the same as 'trash X'
data Effect =
  EffectPlusCards Int |
  EffectPlusActions Int |
  EffectPlusBuys Int |
  EffectPlusMoney Int |
  EffectDiscardNo Int |
  EffectTrashNo Int |

  EffectDiscard Card Location |
  EffectBuy Card |
  EffectGain CardDef Location |
  EffectGainFrom Card Location Location |
  EffectPass Card Location Location |
  EffectPut Card Location Location |
  EffectTrash Card Location |
  EffectReveal Card |
  EffectPlayAction Card |
  EffectPlayCopy Card |
  EffectPlayTreasure Card |
  SpecialEffect CardDef

data Decision =
  Optional Decision Simulation |
  ChooseCard Effect [Card] (Card -> Simulation) |
  ChooseCards Effect [Card] (Int,Int) ([Card] -> Simulation) |
  ChooseToUse Effect (Bool -> Simulation) |
  ChooseToReact Card Trigger (Bool -> Simulation) | -- Or actually, ChooseToReact Trigger Effect, but the original is more descriptive of the decision
  ChooseEffects Int [Effect] ([Effect] -> Simulation)

data DecisionSource = GameMechanics | CardDecision Card

-- TODO add source of decision
data GameStep = State GameState | Decision PlayerId {- DecisionSource -} GameState Decision

type Action = PlayerId -> GameState -> Simulation


instance Show GameState where
  show g = "Game {\n" ++
    "  turn: " ++ (show (turnNo g)) ++ " (ply: " ++ (show (ply g)) ++ ")\n" ++
    "  players: {\n" ++
    concatMap
      (\p ->
        "    " ++ name p ++ ": {\n" ++
        "      hand:    [ " ++ summarizeCards (hand p) ++ " ]\n" ++
        "      play:    [ " ++ summarizeCards (inPlay p) ++ " ]\n" ++
        "      deck:    [ " ++ summarizeCards (deck p) ++ " ]\n" ++
        "      discard: [ " ++ summarizeCards (discardPile p) ++ " ]\n" ++
        "      points:  " ++ show (points p) ++ "\n" ++
        "    }\n")
      (players g) ++
    "  }\n" ++
    "  supply: [ " ++
    (summarizeCards $ concatMap snd (Map.toList (piles g))) ++
    " ]\n" ++
    "  turn: { " ++
    "actions: " ++ show (actions (turn g)) ++ ", buys: " ++ show (buys (turn g)) ++ ", money: " ++ show (money (turn g)) ++
    " }\n" ++
    "}"

instance Show Effect where
  show (EffectPlusCards no) = "+" ++ show no ++ " card(s)"
  show (EffectPlusActions no) = "+" ++ show no ++ " action(s)"
  show (EffectPlusBuys no) = "+" ++ show no ++ " buy(s)"
  show (EffectPlusMoney no) = "+" ++ show no ++ " money"
  show (EffectDiscardNo no) = "discard " ++ show no ++ " card(s)"
  show (EffectTrashNo no) = "trash " ++ show no ++ " card(s)"
  show (EffectDiscard card from) = "discard " ++ show card ++ " from " ++ show from
  show (EffectBuy card) = "buy " ++ show card
  show (EffectGain card to) = "gain " ++ show card ++ " to " ++ show to
  show (EffectGainFrom card from to) = "gain " ++ show card ++ " from " ++ show from ++ " to " ++ show to
  show (EffectPass card from to) = "pass " ++ show card ++ " from " ++ show from ++ " to " ++ show to
  show (EffectPut card from to) = "put " ++ show card ++ " from " ++ show from ++ " to " ++ show to
  show (EffectTrash card from) = "trash " ++ show card ++ " from " ++ show from
  show (EffectReveal card) = "reveal " ++ show card
  show (EffectPlayAction card) = "play " ++ show card
  show (EffectPlayCopy card) = "play copy of " ++ show card
  show (EffectPlayTreasure card) = "play treasure " ++ show card
  show (SpecialEffect card) = "use ability of " ++ show card



-- Game creation

mkPlayer :: [Card] -> String -> SimulationT Player
mkPlayer deck name = draw Player { name = name, hand = [],
                                   discardPile = deck, deck = [], inPlay = [],
                                   mats = Map.empty, inPlayDuration = [],
                                   tokens = Map.empty }
                          5

initialDeck :: [CardDef]
initialDeck = replicate 7 copper ++ replicate 3 estate

labelCards :: [CardDef] -> Int -> ([Card],Int)
labelCards cards initialId = (zipWith Card [initialId..(initialId + length cards)] cards, initialId + length cards)

nullPlayer :: Player
nullPlayer = Player { name = "Alice", hand = [], discardPile = [], deck = [], inPlay = [],
                      mats = Map.empty, inPlayDuration = [], tokens = Map.empty }

nullState :: GameState
nullState =
  GameState { players = Map.singleton "Alice" nullPlayer,
              turnOrder = ["Alice"],
              piles = Map.empty,
              trashPile = [],
              turn = newTurn,
              ply = 1,
              finished = False }

mkGame :: GameType -> [String] -> [CardDef] -> SimulationT GameState
mkGame typ names kingdomCards =
  (sequence $ zipWith mkPlayer decks names) >>= \players ->
    return $
    GameState { players = Map.fromList $ zip names players,
                turnOrder = names,
                piles = pileMap,
                trashPile = [],
                turn = newTurn,
                ply = 1,
                finished = False
                }
  where
    -- Map.fromList $ map (\c -> (c, initialSupply c playerNo)) (standardCards ++ kingdomCards),
    (pileMap,outId) = foldr
                        (\c (m,id) -> let (cs,id2) = labelCards (replicate (initialSupply c playerNo) c) id
                                      in (Map.insert c cs m, id2))
                        (Map.empty,0)
                        (standardCards ++ kingdomCards)
    (decks,_) = foldr (\d (ds,id) -> let (d',id') = labelCards d id
                                     in (d':ds,id'))
                  ([],outId)
                  (replicate playerNo initialDeck)
    standardCards = colonyCards ++ potionCards ++ [estate,duchy,province,copper,silver,gold,curse]
    colonyCards = if typ == ColonyGame then [platinum, colony] else []
    potionCards = if any ((0<) . potionCost . cost nullState) kingdomCards then [potion] else []
    playerNo = length names


-- Combinators

decision :: Decision -> Action
decision decision player state = return $ Decision player state decision

optDecision :: Decision -> Action
optDecision decision player state = return $ Decision player state (Optional decision (return (State state)))

andThenI :: Decision -> (GameState -> Simulation) -> Decision
andThenI (ChooseToReact caption card cont) f = ChooseToReact caption card (\b -> cont b `andThen` f)
andThenI (ChooseToUse effect cont) f = ChooseToUse effect (\b -> cont b `andThen` f)
andThenI (ChooseCard caption cards cont) f = ChooseCard caption cards (\c -> cont c `andThen` f)
andThenI (ChooseCards caption cards lohi cont) f = ChooseCards caption cards lohi (\cs -> cont cs `andThen` f)
andThenI (ChooseEffects num effects cont) f = ChooseEffects num effects (\cs -> cont cs `andThen` f)
andThenI (Optional inner fallback) f = Optional (inner `andThenI` f) (fallback `andThen` f)

andThen :: Simulation -> (GameState -> Simulation) -> Simulation
andThen sim f = do
  step <- sim
  case step of
    (State state) -> f state
    (Decision player state choice) -> decision (choice `andThenI` f) player state

toAction :: GameStep -> Action
toAction step _ _ = return step

toSimulation :: GameState -> Simulation
toSimulation state = return (State state)

{-# INLINE (&&&) #-}
(&&&) :: Action -> Action -> Action
(&&&) act1 act2 player state = act1 player state `andThen` act2 player

{-# INLINE (&&+) #-}
(&&+) :: Action -> (PlayerId -> GameState -> Action) -> Action
(&&+) act1 act2 player state = act1 player state `andThen` (\s2 -> (act2 player s2) player s2)

{-# INLINE (&&=) #-}
(&&=) :: (a -> Action) -> a -> Action
(&&=) = ($)

chooseOne :: Effect -> [Card] -> (Card -> Action) -> Action
chooseOne typ choices cont player state =
  decision (ChooseCard typ choices (\card -> cont card player state)) player state

chooseMany :: Effect -> [Card] -> (Int,Int) -> ([Card] -> Action) -> Action
chooseMany typ choices lohi cont player state =
  decision (ChooseCards typ choices lohi (\chosen -> cont chosen player state)) player state

chooseEffects :: Int -> [Effect] -> ([Effect] -> Action) -> Action
chooseEffects num effects cont player state =
  decision (ChooseEffects num effects (\chosen -> cont chosen player state)) player state

-- Game functions

moveTo :: Card -> Location -> GameState -> GameState
moveTo c (Hand player) state      = updatePlayer state player (\p -> p { hand = c : hand p })
moveTo c (Discard player) state   = updatePlayer state player (\p -> p { discardPile = c : discardPile p })
moveTo c (TopOfDeck player) state = updatePlayer state player (\p -> p { deck = c : deck p })
moveTo c InPlay state             = updatePlayer state (name (activePlayer state)) (\p -> p { inPlay = c : inPlay p })
moveTo c InPlayDuration state     = updatePlayer state (name (activePlayer state)) (\p -> p { inPlayDuration = (Left c) : inPlayDuration p })
moveTo c Trash state              = state { trashPile = c : trashPile state }
moveTo c Supply state             = state { piles = Map.adjust (c:) (typ c) (piles state) }
moveTo c (Mat player mat) state   = updatePlayer state player (\p -> p { mats = Map.insertWith (++) mat [c] (mats p) })

moveFrom :: Card -> Location -> GameState -> GameState
moveFrom c (Hand player) state      = updatePlayer state player (\p -> p { hand = L.delete c $ hand p })
moveFrom c (Discard player) state   = updatePlayer state player (\p -> p { discardPile = L.delete c $ discardPile p })
moveFrom c (TopOfDeck player) state = updatePlayer state player (\p -> p { deck = L.delete c $ deck p })
moveFrom c InPlay state             = updatePlayer state (name (activePlayer state)) (\p -> p { inPlay = L.delete c $ inPlay p })
moveFrom c InPlayDuration state     = updatePlayer state (name (activePlayer state)) (\p -> p { inPlayDuration = L.delete (Left c) $ inPlayDuration p })
moveFrom c Trash state              = state { trashPile = L.delete c $ trashPile state }
moveFrom c Supply state             = state { piles = Map.adjust tail (typ c) (piles state) }
moveFrom c (Mat player mat) state   = updatePlayer state player (\p -> p { mats = Map.adjust (L.delete c) mat (mats p) })

addDurationEffect :: CardDef -> GameState -> GameState
addDurationEffect def state =
  updatePlayer state (name (activePlayer state)) (\p -> p { inPlayDuration = (Right def) : inPlayDuration p })

transfer :: Card -> Location -> Location -> GameState -> GameState
transfer c from to state = moveTo c to (moveFrom c from state)

updatePlayer :: GameState -> String -> (Player -> Player) -> GameState
updatePlayer state pname f = state { players = Map.adjust f pname (players state) }

updatePlayerR :: GameState -> String -> (Player -> SimulationT Player) -> SimulationT GameState
updatePlayerR state pname f =
  f ((players state) Map.! pname) >>= \new ->
  return (state { players = Map.insert pname new (players state) })


-- Queries and predicates

activePlayer :: GameState -> Player
activePlayer state = (players state) Map.! (activePlayerId state)

activePlayerId :: GameState -> PlayerId
activePlayerId = head . turnOrder

playerByName :: GameState -> PlayerId -> Player
playerByName state pname = (players state) Map.! pname

playerNames :: GameState -> [String]
playerNames = turnOrder

opponentNames :: GameState -> String -> [String]
opponentNames state player = map name $ opponents state player

opponents :: GameState -> String -> [Player]
opponents state player = filter ((/= player) . name) $ Map.elems $ players state


availableCards :: GameState -> [CardDef]
availableCards g = Map.keys $ Map.filter (not . null) (piles g)

supply :: GameState -> [(CardDef, Int)]
supply state = Map.toList $ Map.map length $ piles state

inSupply :: GameState -> CardDef -> Bool
inSupply state card = maybe False (not . null) (Map.lookup card (piles state))

numInSupply :: GameState -> CardDef -> Int
numInSupply state card = maybe 0 length (Map.lookup card (piles state))

hasType :: CardDef -> CardType -> Bool
hasType card typ = typ `elem` types card

hasCardType :: Card -> CardType -> Bool
hasCardType card intyp = intyp `elem` types (typ card)

isAction card = hasCardType card Action
isReaction card = hasCardType card Reaction
isTreasure card = hasCardType card Treasure
isDuration card = hasCardType card Duration
isVictory card = hasCardType card Victory
isAttack card = hasCardType card Attack

cost :: GameState -> CardDef -> Cost
cost state card = Cost ((applyMod mod state m),p)
  where
    mod = foldr stackMod (currentModifier state (ModCost Nothing))
            (map (\t -> currentModifier state (ModCost (Just t))) (types card))
    (Cost (m,p)) = cardCost card

affordableCards :: Cost -> GameState -> [CardDef]
affordableCards bound state =
  filter ((`smallerEqCost` bound) . cost state) (availableCards state)

affordableCardsM :: Int -> GameState -> [CardDef]
affordableCardsM num = affordableCards (simpleCost num)

currentModifier :: GameState -> ModAttribute -> Modifier
currentModifier state attr =
  Map.findWithDefault NullModifier attr (modifiers (turn state))

allCards :: Player -> [Card]
allCards s = concatMap (\f -> f s) [hand, inPlay, Either.lefts . inPlayDuration, discardPile, deck]

points :: Player -> Int
points p = pcards + ptokens
  where
    pcards = sum $ map ((`cardPoints` p) . typ) $ allCards p
    ptokens = Map.findWithDefault 0 VictoryToken (tokens p)

turnNo :: GameState -> Int
turnNo g = ((ply g + 1) `div` length (players g))

unknownDef = CardDef (-1) "XXX" Base (simpleCost 0) [] (\_ -> 0) (\_ -> pass) (const 0) noTriggers (const False)
unknown = Card (-1) unknownDef

-- Removes invisble information from the state such as opponents hands
-- It assumes some intelligent information retention such as about own deck content
-- but more could be done for opponents
visibleState :: PlayerId -> GameState -> GameState
visibleState id state = state { players = Map.map anonymize (players state) }
  where
    anonymize p
      | id == name p = p { deck = L.sort (deck p) }
      | otherwise = p { hand = replicate (length (hand p)) unknown,
                        deck = replicate (length (deck p)) unknown }


-- Utilities

-- TODO surprisingly shuffling is one of the most time-intensive bits and the libraries
-- don't seem to be too fast :(
-- see also: https://wiki.haskell.org/Random_shuffle
shuffle' :: [a] -> StdGen -> ([a],StdGen)
shuffle' xs gen = runST (do
        g <- newSTRef gen
        let randomRST lohi = do
              (a,s') <- liftM (randomR lohi) (readSTRef g)
              writeSTRef g s'
              return a
        ar <- newArray n xs
        xs' <- forM [1..n] $ \i -> do
                j <- randomRST (i,n)
                vi <- readArray ar i
                vj <- readArray ar j
                writeArray ar j vi
                return vj
        gen' <- readSTRef g
        return (xs',gen'))
  where
    n = length xs
    newArray :: Int -> [a] -> ST s (STArray s Int a)
    newArray n xs =  newListArray (1,n) xs

shuffle :: [a] -> SimulationT [a]
shuffle xs = randomGen >>= \gen -> let (cards,gen') = shuffle' xs gen in setRandomGen gen' >> return cards

-- Clojure / F# style threading
{-# INLINE (|>) #-}
(|>) :: a -> (a -> b) -> b
(|>) x f = f x

summarizeCards :: [Card] -> String
summarizeCards cards =
  cards |>
  L.sort |>
  L.group |>
  L.map (\cs -> (if (length cs > 1) then show (length cs) ++ " " else "") ++ show (head cs)) |>
  L.intersperse ", " |>
  concat

reshuffleDiscard :: Player -> SimulationT Player
reshuffleDiscard p =
  do
    shuffled <- shuffle (discardPile p)
    return (p { discardPile = [], deck = deck p ++ shuffled })

canDraw :: Player -> Bool
canDraw p = not (null (hand p)) || not (null (discardPile p))

draw :: Player -> Int -> SimulationT
 Player
draw p num
  | length (deck p) < num = fmap (\p -> p !!! num) (reshuffleDiscard p)
  | otherwise = return $ p !!! num
  where
    (!!!) p num = p { hand = take num (deck p) ++ hand p, deck = drop num (deck p)}

ensureCanDraw :: Int -> GameState -> PlayerId -> SimulationT (Maybe GameState)
ensureCanDraw num state name
  | length (deck (playerByName state name)) >= num = return $ Just state
  | otherwise =
    updatePlayerR state name reshuffleDiscard >>= \s2 ->
    return (if length (deck (playerByName s2 name)) >= num then Just s2 else Nothing)

-- Game action

addModifier :: ModAttribute -> Modifier -> Action
addModifier attr mod _ state =
  toSimulation $ state { turn = (turn state) { modifiers = Map.insertWith stackMod attr mod (modifiers (turn state)) } }

playEffect :: CardDef -> Maybe Card -> Action
playEffect card source player state =
  info AllPlayers (player ++ " plays " ++ cardName card)
  >> onPlay card source player (if Maybe.isNothing source && hasType card Duration
                                then addDurationEffect card state
                                else state)

play :: Card -> Action
play card player state =
  playEffect (typ card)
             (Just card)
             player
             (transfer card (Hand player) (if isDuration card then InPlayDuration else InPlay) state)

playAll :: [Card] -> Action
playAll [] _ state = toSimulation state
playAll (c:cs) player state = play c player state `andThen` playAll cs player

-- partial function, check supply first!
topOfSupply :: CardDef -> GameState -> Card
topOfSupply card state = head $ (piles state) Map.! card

gainFrom :: Card -> Location -> Action
gainFrom card source player state =
  info AllPlayers (player ++ " gains " ++ cardName (typ card)) >> return (State $ transfer card source (Discard player) state)

gainTo :: CardDef -> Location -> Action
gainTo card target player state
  | inSupply state card =
    info AllPlayers (player ++ " gains " ++ cardName card) >> return (State $ transfer c Supply target state)
  | otherwise = toSimulation state
  where
    c = topOfSupply card state

gain :: CardDef -> Action
gain card player = gainTo card (Discard player) player

reveal :: [Card] -> Action
reveal cards player state = info AllPlayers (player ++ " reveals " ++ summarizeCards cards) >> return (State state)

buy :: CardDef -> Action
buy card player state =
  info AllPlayers (player ++ " buys " ++ cardName card) >>
    maybe
      (transferT state)
      (\trigger -> trigger player state `andThen` transferT)
      (Map.lookup BuyTrigger (triggers card))
  where
    transferT state = toSimulation $ transfer (topOfSupply card state) Supply (Discard player) state

trash :: Card -> Location -> Action
trash card source player state =
  info AllPlayers (player ++ " trashes " ++ cardName (typ card)) >>
    maybe
      (transferT state)
      (\trigger -> trigger player state `andThen` transferT)
      (Map.lookup TrashTrigger (triggers (typ card)))
  where
    transferT state = toSimulation $ transfer card source Trash state

put :: Card -> Location -> Location -> Action
put card source target _ state = toSimulation $ transfer card source target state

trashAll :: [Card] -> Location -> Action
trashAll cards source player state =
  info AllPlayers (player ++ " trashes " ++ summarizeCards cards) >>
    maybe
      (transferT player state)
      (\actions -> (foldr (&&&) transferT actions) player state)
      ts
  where
    transferT _ state = toSimulation $ foldr (\c s -> transfer c source Trash s) state cards
    ts = sequence $ map ((Map.lookup TrashTrigger) . triggers . typ) cards

discard :: Card -> Location -> Action
discard card loc player state =
  info AllPlayers (player ++ " discards " ++ cardName (typ card)) >> return (State $ transfer card loc (Discard player) state)

plusMoney :: Int -> Action
plusMoney num _ state = toSimulation $ state { turn = (turn state) { money = num + money (turn state) } }

plusPotion :: Action
plusPotion _ state = toSimulation $ state { turn = (turn state) { potions = 1 + potions (turn state) } }

plusCards :: Int -> Action
plusCards num player state =
  do
    s2 <- updatePlayerR state player $ \p -> draw p num
    let newhand = hand (playerByName s2 player)
    let newcards = take (length newhand - length (hand (playerByName state player))) newhand
    info (VisibleToPlayer player) ("Drew " ++ summarizeCards newcards)
    toSimulation s2

plusBuys :: Int -> Action
plusBuys num _ state = toSimulation $ state { turn = (turn state) { buys = num + buys (turn state) } }

plusActions :: Int -> Action
plusActions num _ state = toSimulation $ state { turn = (turn state) { actions = num + actions (turn state) } }

plusTokens :: Int -> Token -> Action
plusTokens num token player state = toSimulation $ updatePlayer state player $ \p -> p { tokens = Map.insertWith (+) token num (tokens p)}

pass :: Action
pass _ = toSimulation

-- Macro flows

newTurn :: TurnState
newTurn = TurnState { money = 0, buys = 1, actions = 1, potions = 0, modifiers = Map.empty }

nextTurn :: GameState -> SimulationT GameState
nextTurn state = nnext
  where
    current = head (turnOrder state)
    next = state { turn = newTurn,
                   turnOrder = tail (turnOrder state) ++ [current],
                   ply = 1 + ply state}

    -- TODO can cleanup trigger anything like discard ?
    nnext = updatePlayerR next current (\p -> draw p { hand = [], inPlay = [], discardPile = hand p ++ inPlay p ++ discardPile p } 5)

checkFinished :: GameState -> Bool
checkFinished state =
  numInSupply state province == 0
  || Map.size (Map.filter null (piles state)) >= 3

-- TODO island mat should be handled by and end game trigger of sorts
endGame :: GameState -> GameState
endGame state = state { finished = True, players = Map.map clearIslandMat (players state) }
  where
    clearIslandMat player = player { deck = deck player ++ Map.findWithDefault [] IslandMat (mats player),
                                     mats = Map.delete IslandMat (mats player)
                                     }

cleanupPhase :: Action
cleanupPhase _ state = M.liftM State (nextTurn (if done then endGame state else state))
  where
    done = checkFinished state

buyPhase :: Action
buyPhase name state
  | buys (turn state) == 0 = toSimulation state
  | otherwise = buyDecision state
  where
    buyDecision s2 = optDecision (ChooseCard (EffectBuy unknown)
                                      candidates
                                      (\card -> (plusBuys (-1) &&& payCosts (cost s2 (typ card)) &&& buy (typ card) &&& buyPhase) name s2))
                                    name s2
      where
        payCosts cost _ state = toSimulation $
          state { turn = t { money = money t - moneyCost cost, potions = potions t - potionCost cost }}
          where
            t = turn state
        moneyToSpend = money (turn s2)
        potionToSpend = potions (turn s2)
        candidates = map (`topOfSupply` s2) $ filter (`canBuy` s2) $ affordableCards (fullCost moneyToSpend potionToSpend) s2

playTreasures :: Action
playTreasures name state
  | length treasures > 0 = playTreasureDecision
  | otherwise = toSimulation state
  where
    treasures = filter isTreasure (hand (activePlayer state))
    playTreasureDecision = optDecision (ChooseCards (EffectPlayTreasure unknown)
                                             treasures
                                             (0,length treasures)
                                             (\cards -> playAll cards name state))
                            name state


actionPhase :: Action
actionPhase name state
  | actions (turn state) == 0 || null actionsInHand = toSimulation state
  | otherwise = optDecision (ChooseCard (EffectPlayAction unknown) actionsInHand (\card -> (plusActions (-1) &&& play card &&& actionPhase) name state))
                         name state
  where
    actionsInHand = filter isAction (hand (activePlayer state))

startOfTurn :: Action
startOfTurn player state
  | null durations = toSimulation state
  | otherwise = allTriggers player (updatePlayer state player (const p { inPlayDuration = [], inPlay = durationCards ++ inPlay p }))
  where
    trigger (Left card) = Map.findWithDefault pass StartOfTurnTrigger (triggers (typ card))
    trigger (Right def) = Map.findWithDefault pass StartOfTurnTrigger (triggers def)
    allTriggers :: Action
    allTriggers = foldr (&&&) pass (map trigger durations)
    p = playerByName state player
    durations = inPlayDuration p
    durationCards = Either.lefts durations

playTurn :: Action
playTurn name state =
  info AllPlayers ("Turn " ++ show (turnNo state) ++ " - " ++ name)
  >> (startOfTurn &&& actionPhase &&& playTreasures &&& buyPhase &&& cleanupPhase) name state

winner :: GameState -> Result
winner state
  | rank one two == EQ = Tie (L.sort $ map name $ takeWhile ((==EQ) . rank one) ranked)
  | otherwise = Win (name one)
  where
    ranked@(one:two:_) = players state |> Map.elems |> L.sortBy rank |> reverse
    rank p1 p2
      | pcompare == EQ = compare (hadFinalTurn p2) (hadFinalTurn p1)
      | otherwise = pcompare
      where
        pcompare = compare (points p1) (points p2)

    hadFinalTurn player = (name player) `elem` playersHadFinalTurn

    playersHadFinalTurn = drop (noPlayers - ((ply state + noPlayers - 1) `mod` noPlayers)) $ turnOrder state
    noPlayers = length $ players state


-- Simulation Stepping

evalSim :: SimulationT a -> StdGen -> a
evalSim sim gen = St.evalState sim (gen,[])

runSim :: SimulationT a -> StdGen -> (a,SimulationState)
runSim sim gen = (res,(resgen,reverse infos))
  where
    (res,(resgen,infos)) = St.runState sim (gen,[])

sim2Gen :: SimulationState -> StdGen
sim2Gen = fst

sim2Infos :: SimulationState -> [Info]
sim2Infos = snd

combineSimStates :: SimulationState -> SimulationState -> SimulationState
combineSimStates (_,infosA) (genB, infosB) = (genB, infosA ++ infosB)
