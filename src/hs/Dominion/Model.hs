{-# LANGUAGE TupleSections #-}
module Dominion.Model where

import qualified Control.Monad.Trans.State.Lazy as St
import qualified Data.Either as Either
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import System.Random (StdGen, randomR)

import Data.Array.ST
import Control.Applicative ()
import Control.Monad
import Control.Monad.ST
import Data.STRef

data CardType = Action | Treasure | Victory | CurseType {- This is a bit of a hack -} |
    Reaction | Attack | Duration |
    Prize | Looter | Ruins | Shelter | Knight | Reserve | Traveller
    deriving (Eq, Ord, Read, Show)

data Location = Hand PlayerId | Discard PlayerId | TopOfDeck PlayerId | BottomOfDeck PlayerId |
  InPlay | InPlayDuration | Trash | Supply | NonSupply | Mat PlayerId Mat | Aside
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
                   cardCost :: GameState -> Cost,
                   types :: ![CardType],
                   cardPoints :: !(Player -> Int),
                   onPlay :: !(Maybe Card -> ActionTemplate),
                   initialSupply :: Int -> Int,
                   triggers :: TriggerHandler,
                   canBuy :: GameState -> Bool
                   }

implemented :: CardDef -> Bool
implemented = (>=0) . cardTypeId

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

data TriggerSource = FromCard Card Location |
                     FromCardEffect CardDef |
                     FromSupply CardDef

type TriggerHandler = Trigger -> Effect -> TriggerSource -> ActionTemplate -> ActionTemplate

nullHandler :: TriggerHandler
nullHandler _ _ _ cont = cont

whileInPlay :: TriggerHandler -> TriggerHandler
whileInPlay inner trigger effect (FromCard card loc) cont player = gameState' >>= sub
  where
    sub state
      | card `elem` inp = inner trigger effect (FromCard card loc) cont player
      | otherwise = cont player
      where
        inp = inPlay $ playerByName state player
whileInPlay _ _ _ _ cont player = cont player

whileInHand :: TriggerHandler -> TriggerHandler
whileInHand inner trigger effect (FromCard card loc) cont player = gameState' >>= sub
  where
    sub state
      | card `elem` inp = inner trigger effect (FromCard card loc) cont player
      | otherwise = cont player
      where
        inp = hand $ playerByName state player
whileInHand _ _ _ _ cont player = cont player

combineHandlers :: TriggerHandler -> TriggerHandler -> TriggerHandler
combineHandlers h1 h2 trigger effect source cont = h1 trigger effect source (h2 trigger effect source cont)

onDiscardFromPlay :: (Card -> ActionTemplate -> ActionTemplate) -> TriggerHandler
onDiscardFromPlay act DiscardTrigger (EffectDiscard c1 InPlay) (FromCard c2 _) cont = if c1 == c2 then act c2 cont else cont
onDiscardFromPlay _ _ _ _ cont = cont

onDiscardSelf :: (Card -> ActionTemplate) -> TriggerHandler
onDiscardSelf action DiscardTrigger (EffectDiscard c1 _) (FromCard c2 _) cont = if c1 == c2 then action c2 &&& cont else cont
onDiscardSelf _ _ _ _ cont = cont

onBuyA :: (TriggerSource -> CardDef -> ActionTemplate) -> TriggerHandler
onBuyA action BuyTrigger (EffectBuy def) source cont = (action source def) &&& cont
onBuyA _ _ _ _ cont = cont

onBuy :: (CardDef -> ActionTemplate) -> TriggerHandler
onBuy action = onBuyA (\_ -> action)

onBuySelf :: ActionTemplate -> TriggerHandler
onBuySelf action BuyTrigger (EffectBuy c1) (FromCardEffect c2) cont = if c1 == c2 then action &&& cont else cont
onBuySelf _ _ _ _ cont = cont

onGainSelf :: ActionTemplate -> TriggerHandler
onGainSelf action GainTrigger (EffectGainFrom c1 _ _) (FromCard c2 _) cont = if c1 == c2 then action &&& cont else cont
onGainSelf action GainTrigger (EffectGain d1 _) (FromCardEffect d2) cont = if d1 == d2 then action &&& cont else cont
onGainSelf _ _ _ _ cont = cont

onTrashSelf :: ActionTemplate -> TriggerHandler
onTrashSelf action TrashTrigger (EffectTrash c1 _) (FromCard c2 _) cont = if c1 == c2 then action &&& cont else cont
onTrashSelf _ _ _ _ cont = cont

onTrash :: ActionTemplate -> TriggerHandler
onTrash action TrashTrigger _ _ cont = cont &&& action
onTrash _ _ _ _ cont = cont

onStartOfTurn :: ActionTemplate -> TriggerHandler
onStartOfTurn action StartOfTurnTrigger _ _ cont = action &&& cont
onStartOfTurn _ _ _ _ cont = cont

onStartOfGame :: ActionTemplate -> TriggerHandler
onStartOfGame action StartOfGameTrigger _ _ cont = action &&& cont
onStartOfGame _ _ _ _ cont = cont

onAttack :: ActionTemplate -> TriggerHandler
onAttack action AttackTrigger _ _ cont = action &&& cont
onAttack _ _ _ _ cont = cont

onAttackA :: (TriggerSource -> ActionTemplate) -> TriggerHandler
onAttackA action AttackTrigger _ source cont = (action source) &&& cont
onAttackA _ _ _ _ cont = cont


-- Card definition helpers

noPoints :: a -> Int
noPoints = const 0

noTriggers :: TriggerHandler
noTriggers = nullHandler

-- TODO refactor these to use carddef* instead
carddef :: Int -> String -> Cost -> [CardType] -> (Player -> Int) -> ActionTemplate -> TriggerHandler -> Edition -> CardDef
carddef id name cost types points effect triggers edition = CardDef id name edition (const cost) types points (\_ -> effect) (const 10) triggers (const True)

carddefA :: Int -> String -> Cost -> [CardType] -> (Player -> Int) -> (Maybe Card -> ActionTemplate) -> TriggerHandler -> Edition -> CardDef
carddefA id name cost types points effect triggers edition = CardDef id name edition (const cost) types points effect (const 10) triggers (const True)

treasure :: Int -> String -> Int -> Int -> Edition -> CardDef
treasure id name cost money edition = CardDef id name edition (const (simpleCost cost)) [Treasure] noPoints (\_ -> plusMoney money) (const 10) noTriggers (const True)

action :: Int -> String -> Int -> ActionTemplate -> Edition -> CardDef
action id name cost effect edition = CardDef id name edition (const (simpleCost cost)) [Action] noPoints (\_ -> effect) (const 10) noTriggers (const True)

actionA :: Int -> String -> Int -> (Maybe Card -> ActionTemplate) -> Edition -> CardDef
actionA id name cost effect edition = CardDef id name edition (const (simpleCost cost)) [Action] noPoints effect (const 10) noTriggers (const True)

duration :: Int -> String -> Int -> ActionTemplate -> ActionTemplate -> Edition -> CardDef
duration id name cost effect startOfTurn edition =
  CardDef id name edition (const (simpleCost cost)) [Action, Duration] noPoints  (\_ -> effect) (const 10) (onStartOfTurn startOfTurn) (const True)

durationA :: Int -> String -> Int -> (Maybe Card -> ActionTemplate) -> ActionTemplate -> Edition -> CardDef
durationA id name cost effect startOfTurn edition =
  CardDef id name edition (const (simpleCost cost)) [Action, Duration] noPoints  effect (const 10) (onStartOfTurn startOfTurn) (const True)

attack :: Int -> String -> Int -> ActionTemplate -> Edition -> CardDef
attack id name cost effect edition = CardDef id name edition (const (simpleCost cost)) [Action, Attack] noPoints (\_ -> effect) (const 10) noTriggers (const True)

victory :: Int -> String -> Int -> Int -> Edition -> CardDef
victory id name cost points edition = CardDef id name edition (const (simpleCost cost)) [Victory] (const points) (\_ -> pass) (const 10) noTriggers (const True)

notImplemented name edition = CardDef (-1) name edition (const (simpleCost 0)) [] (\_ -> 0) (\_ -> pass) (const 0) noTriggers (const False)

withTrigger :: (Edition -> CardDef) -> TriggerHandler -> Edition -> CardDef
withTrigger cardgen trigger ed = card { triggers = trigger }
  where
    card = cardgen ed

withInitialSupply :: (Edition -> CardDef) -> (Int -> Int) -> Edition -> CardDef
withInitialSupply cardgen supplyf ed = (cardgen ed) { initialSupply = supplyf }

withBuyRestriction :: (Edition -> CardDef) -> (GameState -> Bool) -> Edition -> CardDef
withBuyRestriction cardgen pred ed = (cardgen ed) { canBuy = pred }

withSpecialCost :: (Edition -> CardDef) -> (GameState -> Cost) -> Edition -> CardDef
withSpecialCost cardgen f ed = (cardgen ed) { cardCost = f }


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

cHovel = carddef 850 "Hovel" (simpleCost 1) [Reaction, Shelter] noPoints pass (whileInHand (onBuyA hovelTrigger)) DarkAges
cNecropolis = carddef 851 "Necropolis" (simpleCost 1) [Action, Shelter] noPoints (plusActions 2) noTriggers DarkAges
cOvergrownEstate = carddef 852 "Overgrown Estate" (simpleCost 1) [Victory, Shelter] noPoints pass (onTrashSelf (plusCards 1)) DarkAges

hovelTrigger :: TriggerSource -> CardDef -> ActionTemplate
hovelTrigger (FromCard hovel _) card p
  | hasType card Victory = choose (ChooseToReact hovel BuyTrigger) (\b -> if b then trash hovel (Hand p) else pass) p
  | otherwise = noOpSimulation
hovelTrigger _ _ _ = noOpSimulation

shelters = [cHovel, cNecropolis, cOvergrownEstate]

ruinsPseudoDef = carddef (-1) "Ruins" (simpleCost 0) [Action, Ruins] noPoints pass noTriggers DarkAges
ruinsPseudoCard = Card (-1) ruinsPseudoDef

ruins = map ($ DarkAges)
  [carddef 840 "Abandoned Mine" (simpleCost 0) [Action, Ruins] noPoints (plusMoney 1) noTriggers,
   carddef 841 "Ruined Library" (simpleCost 0) [Action, Ruins] noPoints (plusCards 1) noTriggers,
   carddef 842 "Ruined Market" (simpleCost 0) [Action, Ruins] noPoints (plusBuys 1) noTriggers,
   carddef 843 "Ruined Village" (simpleCost 0) [Action, Ruins] noPoints (plusActions 1) noTriggers,
   carddef 844 "Survivors" (simpleCost 0) [Action, Ruins] noPoints (reshuffleIfNeededN 2 &&& survivors) noTriggers
  ]

survivors :: ActionTemplate
survivors p = gameState' >>= inner
  where
    inner s
      | null d = noOpSimulation
      | length d == 1 = choose (ChooseToUse (EffectDiscard (head d) (TopOfDeck p))) (\b -> if b then discard (head d) (TopOfDeck p) else pass) p
      | otherwise = (addLog (LogPeek (VisibleToPlayer p) p [c1,c2] (TopOfDeck p))
                     &&& chooseEffects 1 [EffectDiscard unknown (TopOfDeck p), EffectPut unknown (TopOfDeck p) (TopOfDeck p)] enact)
                      p
      where
        d = deck $ playerByName s p
        [c1,c2] = d
        enact [(EffectDiscard _ _)] = discardAll [c1,c2] (TopOfDeck p)
        enact _ = chooseMany (EffectPut unknown (TopOfDeck p) (TopOfDeck p))
                             [c1,c2]
                             (2,2)
                             (\cs -> putAll (reverse cs) (TopOfDeck p) (TopOfDeck p))



unknownDef = CardDef (-1) "XXX" Base (const (simpleCost 0)) [Action] (\_ -> 0) (\_ -> pass) (const 0) noTriggers (const False)
unknown = Card (-1) unknownDef

isUnknown card = cardName (typ card) == "XXX"


type PlayerId = String

data Mat = IslandMat | NativeVillageMat deriving (Eq, Ord, Show)
data Token = VictoryToken | CoinToken deriving (Eq, Ord, Show)

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

data Visibility = AllPlayers | VisibleToPlayer PlayerId deriving (Eq)

instance Show Visibility where
  show AllPlayers = "All"
  show (VisibleToPlayer player) = player

data Log =
  LogBuy PlayerId CardDef
  | LogPlay PlayerId CardDef
  | LogPut Visibility PlayerId Card Location Location
  | LogTrash PlayerId Card
  | LogDiscard PlayerId [Card]
  | LogGain PlayerId Card Location Location
  | LogReveal PlayerId [Card]
  | LogPeek Visibility PlayerId [Card] Location
  | LogDraw Visibility PlayerId [Card]
  | LogTurn PlayerId Int GameState

instance Show Log where
  show (LogBuy p card) = "[All] " ++ p ++ " buys " ++ show card
  show (LogPlay p card) = "[All] " ++ p ++ " plays " ++ show card
  show (LogPut vis p card source target) = "[" ++ show vis ++ "] " ++ p ++ " puts " ++ show card ++ " from " ++ show source ++ " to " ++ show target
  show (LogTrash p card) = "[All]" ++ p ++ " trashes " ++ show card
  show (LogDiscard p cards) = "[All] " ++ p ++ " discards " ++ summarizeCards cards
  show (LogGain p card source target) = "[All] " ++ p ++ " gains " ++ show card ++ " from " ++ show source ++ " to " ++ show target
  show (LogReveal p cards) = "[All] " ++ p ++ " reveals " ++ summarizeCards cards
  show (LogPeek vis p cards loc) = "[" ++ show vis ++ "] "++ p ++ " finds " ++ summarizeCards cards ++ " at " ++ show loc
  show (LogDraw vis p cards) = "[" ++ show vis ++ "] " ++ p ++ " draws " ++ summarizeCards cards
  show (LogTurn p no _) = "[All] Turn " ++ show no ++ " - " ++ p


canSee :: PlayerId -> Log -> Bool
canSee pid (LogPut (VisibleToPlayer p) _ _ _ _) = pid == p
canSee pid (LogPeek (VisibleToPlayer p) _ _ _) = pid == p
canSee pid (LogDraw (VisibleToPlayer p) _ _) = pid == p
canSee _ _ = True

data Phase = ActionPhase | BuyPhase | CleanupPhase deriving (Eq, Ord, Show)

data TurnState = TurnState { money :: Int,
                             potions :: Int,
                             buys :: Int,
                             actions :: Int,
                             modifiers :: Map.Map ModAttribute Modifier,
                             turnLog :: [Log],
                             phase :: Phase
                             }
                             deriving (Show)


data GameType = StandardGame | ColonyGame | SheltersGame | ColonySheltersGame
  deriving (Eq, Read, Show)

data GameState = GameState { players :: Map.Map PlayerId Player,
                             turnOrder :: [PlayerId],
                             trashPile :: [Card],
                             turn :: TurnState,
                             piles :: Map.Map CardDef [Card],
                             nonSupplyPiles :: Map.Map CardDef [Card],
                             ply :: Int,
                             finished :: Bool,
                             gameLog :: [Log]
                             }

data Result = Tie [PlayerId] | Win PlayerId deriving (Eq, Read, Show)

data SimulationState = SimulationState { game :: GameState, randomGenerator :: StdGen }


-- Notes on the type hierarchy
-- o Base non-monadic type is GameState
-- o Combined with the random generator state, GameState forms the SimulationState
-- o Based on SimulationState we have GameT, which is just a state monad
-- o Finally at the top of the edifice we have SimulationT, which is a monad tracking player decisions
--   and decision dependent continuations. Internally it is uses GameT for actual state tracking

type GameT a = St.State SimulationState a

newtype SimulationT a = SimulationT { nextStep :: GameT (MaybeDecision a (SimulationT a)) }

instance Functor SimulationT where
    fmap f m = m >>= \a -> return (f a)

instance Applicative SimulationT where
    pure v = return v

    (<*>) mf ma = do
        f <- mf
        a <- ma
        return (f a)

instance Monad SimulationT where
    return v = SimulationT $ return (Result v)

    (>>=) (SimulationT dec1) f = SimulationT $
        dec1 >>= \decision ->
            case decision of
                (Result v) -> nextStep (f v)
                (Decision player _ choice) ->
                    gameState >>= \state -> return $ Decision player state $ choice `andThenI` f

andThenI :: Decision (SimulationT a) -> (a -> SimulationT b) -> Decision (SimulationT b)
andThenI (ChooseToReact caption card cont) f = ChooseToReact caption card (\b -> cont b >>= f )
andThenI (ChooseToUse effect cont) f = ChooseToUse effect (\b -> cont b >>= f)
andThenI (ChooseCard caption cards cont) f = ChooseCard caption cards (\c -> cont c >>= f)
andThenI (ChooseCards caption cards lohi cont) f = ChooseCards caption cards lohi (\cs -> cont cs >>= f)
andThenI (ChooseEffects num effects cont) f = ChooseEffects num effects (\cs -> cont cs >>= f)
andThenI (ChooseNumber effect lohi cont) f = ChooseNumber effect lohi (\cs -> cont cs >>= f)
andThenI (Optional inner fallback) f = Optional (inner `andThenI` f) (fallback >>= f)


decision :: Decision (SimulationT ()) -> ActionTemplate
decision decision player = gameState' >>= \state -> SimulationT $ return $ Decision player state decision

optDecision :: Decision (SimulationT ()) -> ActionTemplate
optDecision decision player = gameState' >>= \state ->
    SimulationT $ return $ Decision player state (Optional decision noOpSimulation)

qDecision :: Decision (SimulationT a) -> PlayerId -> SimulationT a
qDecision decision player = gameState' >>= \state -> SimulationT $ return $ Decision player state decision

qOptDecision :: ((a -> SimulationT (Maybe a)) -> Decision (SimulationT (Maybe a))) -> PlayerId -> SimulationT (Maybe a)
qOptDecision decision player = gameState' >>= \state ->
    SimulationT $ return $ Decision player state (Optional (decision (return . Just)) (return Nothing))

qWhenJust :: Maybe a -> (a -> SimulationT ()) -> SimulationT ()
qWhenJust m f = maybe noOpSimulation f m

-- Simulation primitives

randomGen :: GameT StdGen
randomGen = fmap randomGenerator St.get

setRandomGen :: StdGen -> GameT ()
setRandomGen gen = St.modify (\s -> s { randomGenerator = gen })

gameState :: GameT GameState
gameState = fmap game St.get

putGameState :: GameState -> GameT ()
putGameState state = St.modify (\s -> s { game = state})

modGameState :: (GameState -> GameState) -> GameT ()
modGameState f = St.modify (\s -> s { game = f (game s) })

gameState' :: SimulationT GameState
gameState' = SimulationT (fmap Result gameState)

putGameState' :: GameState -> SimulationT ()
putGameState' state = SimulationT (fmap Result (putGameState state))

modGameState' :: (GameState -> GameState) -> SimulationT ()
modGameState' f = SimulationT (fmap Result (modGameState f))

-- TODO add source card
data Trigger =
  AttackTrigger |
  BuyTrigger |
  GainTrigger |
  TrashTrigger |
  DiscardTrigger |
  StartOfTurnTrigger |
  StartOfGameTrigger
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
  EffectBuy CardDef |
  EffectGain CardDef Location |
  EffectGainFrom Card Location Location |
  EffectPass Card Location Location |
  EffectPut Card Location Location |
  EffectTrash Card Location |
  EffectReveal Card |
  EffectPlayAction Card |
  EffectPlayCopy Card |
  EffectPlayTreasure Card |
  EffectUseTokens Token |
  SpecialEffect CardDef |
  MultiEffect [Effect] |
  NullEffect

data Decision a =
  Optional (Decision a) a |
  ChooseCard Effect [Card] (Card -> a) |
  ChooseCards Effect [Card] (Int,Int) ([Card] -> a) |
  ChooseToUse Effect (Bool -> a) |
  ChooseToReact Card Trigger (Bool -> a) | -- Or actually, ChooseToReact Trigger Effect, but the original is more descriptive of the decision
  ChooseEffects Int [Effect] ([Effect] -> a) |
  ChooseNumber Effect (Int,Int) (Int -> a)

instance Show (Decision a) where
  show (Optional dec _) = "Optional(" ++ show dec ++ ")"
  show (ChooseCard effect cards _) = "ChooseCard(" ++ show effect ++ ", " ++ show cards ++ ")"
  show (ChooseCards effect cards (lo,hi) _) = "ChooseCards(" ++ show effect ++ ", " ++ show cards ++ ", " ++ show lo ++ ", " ++ show hi ++ ")"
  show (ChooseToUse effect _) = "ChooseToUse(" ++ show effect ++ ")"
  show (ChooseToReact card trigger _) = "ChooseToReact(" ++ show card ++ ", " ++ show trigger ++ ")"
  show (ChooseEffects num effects _) = "ChooseEffects(" ++ show num ++ ", " ++ show effects ++ ")"
  show (ChooseNumber effect (lo,hi) _) = "ChooseNumber(" ++ show effect ++ ", " ++ show lo ++ ", " ++ show hi ++ ")"

-- TODO add source of decision
data MaybeDecision a b = Result a | Decision PlayerId GameState (Decision b)

type ActionTemplate = PlayerId -> SimulationT ()

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

instance Show a => Show (MaybeDecision a b) where
  show (Result state) = "State(" ++ show state ++ ")"
  show (Decision pid state decision) = "Decision(" ++ show pid ++ ", " ++ show state ++ ", " ++ show decision ++ ")"

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
  show (EffectUseTokens token) = "use token(s) " ++ show token
  show (SpecialEffect card) = "use ability of " ++ show card
  show (MultiEffect effects) = "all of " ++ show effects
  show NullEffect = "no effect"



-- Game creation

mkPlayer :: [Card] -> String -> GameT Player
mkPlayer deck name = draw Player { name = name, hand = [],
                                   discardPile = deck, deck = [], inPlay = [],
                                   mats = Map.empty, inPlayDuration = [],
                                   tokens = Map.empty }
                          5

nullPlayer :: Player
nullPlayer = Player { name = "Alice", hand = [], discardPile = [], deck = [], inPlay = [],
                      mats = Map.empty, inPlayDuration = [], tokens = Map.empty }

nullState :: GameState
nullState =
  GameState { players = Map.singleton "Alice" nullPlayer,
              turnOrder = ["Alice"],
              piles = Map.empty,
              nonSupplyPiles = Map.empty,
              trashPile = [],
              turn = newTurn,
              ply = 1,
              finished = False,
              gameLog = [] }

initialDeck :: [CardDef]
initialDeck = replicate 7 copper ++ replicate 3 estate

sheltersDeck :: [CardDef]
sheltersDeck = replicate 7 copper ++ [cHovel, cOvergrownEstate, cNecropolis]

mkPile :: CardDef -> Int -> [Card]
mkPile card num = map ((`Card` card) . (+ (100 * cardTypeId card))) [0..(num-1)]

mkPlayerPile :: [CardDef] -> Int -> [Card]
mkPlayerPile cards playerNo = zipWith (\c seq -> Card (seq + (100*(20+playerNo))) c) cards [0..]

addNonSupplyPile :: CardDef -> ActionTemplate
addNonSupplyPile card _ =
  modGameState' $ \state -> state { nonSupplyPiles = Map.insert card
                                                    (mkPile card (initialSupply card (length (turnOrder state))))
                                                    (nonSupplyPiles state) }

mkGame :: GameType -> [String] -> [CardDef] -> SimulationT GameState
mkGame typ names kingdomCards =
  SimulationT (liftM Result (sequence $ zipWith mkPlayer decks names))
  >>= (\players -> putGameState' (protoState players))
  >> (handleAllTriggers
        StartOfGameTrigger
        (map FromCardEffect kingdomCards)
        NullEffect
        pass
        (head names))
  >> (if useRuins
      then
        SimulationT (do shuffledRuins <- shuffle allRuins
                        modGameState (\state -> state { piles = Map.insert ruinsPseudoDef (take (10 * (length names - 1)) shuffledRuins) (piles state) })
                        return (Result ()))
      else noOpSimulation)
  >> gameState'

  where
    playerNo = length names

    pileMap = Map.fromList $ map (\c -> (c, mkPile c (initialSupply c playerNo))) $ standardCards ++ kingdomCards

    standardCards = colonyCards ++ potionCards ++ [estate,duchy,province,copper,silver,gold,curse]
    colonyCards = if typ == ColonyGame || typ == ColonySheltersGame then [platinum, colony] else []
    potionCards = if any ((0<) . potionCost . cost nullState) kingdomCards then [potion] else []

    useShelters = typ == SheltersGame || typ == ColonySheltersGame
    useRuins = any (`hasType` Looter) kingdomCards
    allRuins = concatMap (`mkPile` 10) ruins

    deckTemplate = if useShelters then sheltersDeck else initialDeck
    decks = map (mkPlayerPile deckTemplate) [0..(playerNo-1)]

    protoState players = nullState { players = Map.fromList $ zip names players, turnOrder = names, piles = pileMap }

-- Combinators


toAction :: MaybeDecision () (SimulationT ()) -> ActionTemplate
toAction step _ = SimulationT $ return (step)

toSimulation :: GameState -> SimulationT GameState
toSimulation = return

noOpSimulation :: SimulationT ()
noOpSimulation = return ()

simWhen :: Bool -> SimulationT () -> SimulationT ()
simWhen p s = if p then s else noOpSimulation

{-# INLINE (&&&) #-}
(&&&) :: ActionTemplate -> ActionTemplate -> ActionTemplate
(&&&) act1 act2 player = act1 player >> act2 player

{-# INLINE (&&+) #-}
(&&+) :: ActionTemplate -> (PlayerId -> GameState -> ActionTemplate) -> ActionTemplate
(&&+) act1 act2 player = act1 player >> gameState' >>= \state -> act2 player state player

{-# INLINE (&&=) #-}
(&&=) :: (a -> ActionTemplate) -> a -> ActionTemplate
(&&=) = ($)


seqSteps :: ActionTemplate -> [PlayerId] -> SimulationT ()
seqSteps _ [] = noOpSimulation
seqSteps f (x:xs) = f x >> seqSteps f xs

seqActions :: (a -> ActionTemplate) -> [a] -> ActionTemplate
seqActions _ [] _ = noOpSimulation
seqActions f (x:xs) p = f x p >> seqActions f xs p

choose :: ((a -> SimulationT ()) -> Decision (SimulationT ())) -> (a -> ActionTemplate) -> ActionTemplate
choose dec cont player = decision (dec (\input -> cont input player)) player

qChoose :: ((a -> SimulationT a) -> Decision (SimulationT a)) -> PlayerId -> SimulationT a
qChoose dec player = qDecision (dec return) player

chooseOne :: Effect -> [Card] -> (Card -> ActionTemplate) -> ActionTemplate
chooseOne typ choices cont player =
  decision (ChooseCard typ choices (\card -> cont card player)) player

qChooseOne :: Effect -> [Card] -> PlayerId -> SimulationT Card
qChooseOne typ choices player = qDecision (ChooseCard typ choices return) player

chooseMany :: Effect -> [Card] -> (Int,Int) -> ([Card] -> ActionTemplate) -> ActionTemplate
chooseMany typ choices lohi cont player =
  decision (ChooseCards typ choices lohi (\chosen -> cont chosen player)) player

qChooseMany :: Effect -> [Card] -> (Int,Int) -> PlayerId -> SimulationT [Card]
qChooseMany typ choices lohi player = qDecision (ChooseCards typ choices lohi return) player

chooseEffects :: Int -> [Effect] -> ([Effect] -> ActionTemplate) -> ActionTemplate
chooseEffects num effects cont player =
  decision (ChooseEffects num effects (\chosen -> cont chosen player)) player

chooseToReveal :: (CardDef -> Bool) -> ActionTemplate -> ActionTemplate -> ActionTemplate
chooseToReveal selector fyes fno player = gameState' >>= inner
  where
    inner state
      | null cands = fno player
      | otherwise = decision (ChooseToUse (EffectReveal (head cands))
                                          (\b -> if b then (reveal [(head cands)] &&& fyes) player
                                                      else fno player))
                      player
      where
        cands = filter (selector . typ) $ hand $ playerByName state player


-- Game functions

moveTo :: Card -> Location -> GameState -> GameState
moveTo c (Hand player) state         = updatePlayer state player (\p -> p { hand = c : hand p })
moveTo c (Discard player) state      = updatePlayer state player (\p -> p { discardPile = c : discardPile p })
moveTo c (TopOfDeck player) state    = updatePlayer state player (\p -> p { deck = c : deck p })
moveTo c (BottomOfDeck player) state = updatePlayer state player (\p -> p { deck = deck p ++ [c]})
moveTo c InPlay state                = updatePlayer state (name (activePlayer state)) (\p -> p { inPlay = c : inPlay p })
moveTo c InPlayDuration state        = updatePlayer state (name (activePlayer state)) (\p -> p { inPlayDuration = (Left c) : inPlayDuration p })
moveTo c Trash state                 = state { trashPile = c : trashPile state }
moveTo c Supply state                = state { piles = Map.adjust (c:) (if isRuins c then ruinsPseudoDef else (typ c)) (piles state) }
moveTo c NonSupply state             = state { nonSupplyPiles = Map.adjust (c:) (typ c) (nonSupplyPiles state) }
moveTo c (Mat player mat) state      = updatePlayer state player (\p -> p { mats = Map.insertWith (++) mat [c] (mats p) })
-- moving cards to aside puts them nowhere
moveTo _ Aside state                 = state

moveFrom :: Card -> Location -> GameState -> GameState
moveFrom c (Hand player) state         = updatePlayer state player (\p -> p { hand = L.delete c $ hand p })
moveFrom c (Discard player) state      = updatePlayer state player (\p -> p { discardPile = L.delete c $ discardPile p })
moveFrom c (TopOfDeck player) state    = updatePlayer state player (\p -> p { deck = L.delete c $ deck p })
moveFrom c (BottomOfDeck player) state = updatePlayer state player (\p -> p { deck = reverse $ L.delete c $ reverse (deck p)})
moveFrom c InPlay state                = updatePlayer state (name (activePlayer state)) (\p -> p { inPlay = L.delete c $ inPlay p })
moveFrom c InPlayDuration state        = updatePlayer state (name (activePlayer state)) (\p -> p { inPlayDuration = L.delete (Left c) $ inPlayDuration p })
moveFrom c Trash state                 = state { trashPile = L.delete c $ trashPile state }
moveFrom c Supply state                = state { piles = Map.adjust tail (if isRuins c then ruinsPseudoDef else (typ c)) (piles state) }
moveFrom c NonSupply state             = state { nonSupplyPiles = Map.adjust tail (typ c) (nonSupplyPiles state) }
moveFrom c (Mat player mat) state      = updatePlayer state player (\p -> p { mats = Map.adjust (L.delete c) mat (mats p) })
-- moving cards from aside does nothing
moveFrom _ Aside state                 = state

transfer :: Card -> Location -> Location -> GameState -> GameState
transfer c from to state = moveTo c to (moveFrom c from state)

getCards :: Location -> GameState -> [Card]
getCards (Hand player) state = hand $ playerByName state player
getCards (Discard player) state = discardPile $ playerByName state player
getCards (TopOfDeck player) state = take 1 $ deck $ playerByName state player
getCards (BottomOfDeck player) state = take 1 $ reverse $ deck $ playerByName state player
getCards InPlay state = inPlay $ activePlayer state
getCards InPlayDuration state = Either.lefts $ inPlayDuration $ activePlayer state
getCards Trash state = trashPile state
getCards Supply state = concat $ Map.elems (piles state)
getCards NonSupply state = concat $ Map.elems (nonSupplyPiles state)
getCards Aside _ = []
getCards (Mat player mat) state = Map.findWithDefault [] mat (mats $ playerByName state player)

addDurationEffect :: CardDef -> GameState -> GameState
addDurationEffect def state =
  updatePlayer state (name (activePlayer state)) (\p -> p { inPlayDuration = (Right def) : inPlayDuration p })

updatePlayer :: GameState -> String -> (Player -> Player) -> GameState
updatePlayer state pname f = state { players = Map.adjust f pname (players state) }

updatePlayerR :: GameState -> String -> (Player -> GameT Player) -> GameT GameState
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
isRuins card = hasCardType card Ruins
isShelter card = hasCardType card Shelter
isCurse card = hasCardType card CurseType

cardInTableau :: CardDef -> GameState -> Bool
cardInTableau def = Map.member def . piles

cost :: GameState -> CardDef -> Cost
cost state card = Cost ((applyMod mod state m),p)
  where
    mod = foldr stackMod (currentModifier state (ModCost Nothing))
            (map (\t -> currentModifier state (ModCost (Just t))) (types card))
    (Cost (m,p)) = cardCost card state

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

buysThisTurn :: GameState -> [CardDef]
buysThisTurn state = Maybe.catMaybes $ map extract $ turnLog $ turn state
  where
    extract (LogBuy _ c) = Just c
    extract _ = Nothing

playsThisTurn :: GameState -> [CardDef]
playsThisTurn state = Maybe.catMaybes $ map extract $ turnLog $ turn state
  where
    extract (LogPlay _ c) = Just c
    extract _ = Nothing

gameLogs :: GameState -> [Log]
gameLogs = reverse . gameLog

visibleGameLogs :: PlayerId -> GameState -> [Log]
visibleGameLogs pid = reverse . filter (canSee pid) . gameLog


moneyValue :: CardDef -> Int
moneyValue card
  | card == platinum = 5
  | card == gold = 3
  | card == silver = 2
  | card == copper = 1
  | otherwise = 0

moneySum :: [Card] -> Int
moneySum = sum . map (moneyValue . typ)

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

shuffle :: [a] -> GameT [a]
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

reshuffleDiscard :: Player -> GameT Player
reshuffleDiscard p =
  do
    shuffled <- shuffle (discardPile p)
    return (p { discardPile = [], deck = deck p ++ shuffled })

canDraw :: Player -> Bool
canDraw p = not (null (deck p)) || not (null (discardPile p))

draw :: Player -> Int -> GameT Player
draw p num
  | length (deck p) < num = fmap (\p -> p !!! num) (reshuffleDiscard p)
  | otherwise = return $ p !!! num
  where
    (!!!) p num = p { hand = take num (deck p) ++ hand p, deck = drop num (deck p)}



-- Monadic queries

qPlayer :: PlayerId -> SimulationT Player
qPlayer id = gameState' >>= (return . (`playerByName` id))

qHand :: PlayerId -> SimulationT [Card]
qHand = fmap hand . qPlayer

qDeck :: PlayerId -> SimulationT [Card]
qDeck = fmap deck . qPlayer

-- Game action

addModifier :: ModAttribute -> Modifier -> ActionTemplate
addModifier attr mod _ = modGameState' $ \state -> state { turn = (turn state) { modifiers = Map.insertWith stackMod attr mod (modifiers (turn state)) } }

addLog :: Log -> ActionTemplate
addLog item _ = modGameState' $ \state -> state { turn = (turn state) { turnLog = item : turnLog (turn state) }, gameLog = item : (gameLog state) }

playEffect :: CardDef -> Maybe Card -> ActionTemplate
playEffect card source player =
  modGameState' (\state -> if Maybe.isNothing source && hasType card Duration
                              then addDurationEffect card state
                              else state) >>
  (addLog (LogPlay player card) &&& onPlay card source)
  player

playFrom :: Card -> Location -> ActionTemplate
playFrom card loc player =
  modGameState' (transfer card loc (if isDuration card then InPlayDuration else InPlay)) >>
  playEffect (typ card)
             (Just card)
             player

play :: Card -> ActionTemplate
play card player = playFrom card (Hand player) player

playAll :: [Card] -> ActionTemplate
playAll [] _ = noOpSimulation
playAll (c:cs) player = play c player >> playAll cs player

getTrigger :: TriggerSource -> Trigger -> Effect -> ActionTemplate -> ActionTemplate
getTrigger (FromCard card loc) trigger effect cont = (triggers (typ card)) trigger effect (FromCard card loc) cont
getTrigger (FromCardEffect def) trigger effect cont = (triggers def) trigger effect (FromCardEffect def) cont
getTrigger (FromSupply def) trigger effect cont = (triggers def) trigger effect (FromSupply def) cont

-- partial function, check supply first!
topOfSupply :: CardDef -> GameState -> Card
topOfSupply card state = head $ (piles state) Map.! card

handleTriggers :: Trigger -> TriggerSource -> Effect -> ActionTemplate -> ActionTemplate
handleTriggers trigger card = getTrigger card trigger

handleAllTriggers :: Trigger -> [TriggerSource] -> Effect -> ActionTemplate -> ActionTemplate
handleAllTriggers trigger cards effect cont =
  foldr (\c cont -> getTrigger c trigger effect cont)
        cont
        cards

reshuffle :: ActionTemplate
reshuffle player =
    gameState' >>= \state ->
        SimulationT (liftM Result (reshuffleDiscard (playerByName state player)
                                   >>= (\p -> return $ state { players = Map.insert player p (players state) })
                                   >>= putGameState))


reshuffleIfNeeded :: ActionTemplate
reshuffleIfNeeded player = gameState' >>= \state -> if null (deck (playerByName state player)) then reshuffle player else noOpSimulation

reshuffleIfNeededN :: Int -> ActionTemplate
reshuffleIfNeededN n player = gameState' >>= \state -> if length (deck (playerByName state player)) < n then reshuffle player else noOpSimulation

-- same as put but without the extra logging
move :: Card -> Location -> Location -> ActionTemplate
move card source target _ = modGameState' $ transfer card source target

put :: Card -> Location -> Location -> ActionTemplate
put card source target p =
  (addLog (LogPut (VisibleToPlayer p) p card source target) &&& move card source target) p

putAll :: [Card] -> Location -> Location -> ActionTemplate
putAll cards source target = seqActions (\c -> put c source target) cards

playerTriggers :: Player -> [TriggerSource]
playerTriggers p = map (`FromCard` (Hand (name p))) (hand p) ++ map (`FromCard` InPlay) (inPlay p)

supplyTriggers :: GameState -> [TriggerSource]
supplyTriggers state = map FromSupply $ Map.keys (piles state)

gainFromTo :: Card -> Location -> Location -> ActionTemplate
gainFromTo card source target player =
  gameState' >>= \state ->
      (addLog (LogGain player card source target)
       &&& handleAllTriggers GainTrigger
            ((FromCard card source):playerTriggers (playerByName state player) ++ supplyTriggers state)
            (EffectGainFrom card source target)
            (move card source target))
        player

gainFrom :: Card -> Location -> ActionTemplate
gainFrom card source player = gainFromTo card source (Discard player) player

gainSpecial :: CardDef -> Location -> ActionTemplate
gainSpecial card target player = gameState' >>= inner
  where
    inner state
      | null pile = noOpSimulation
      | otherwise = (addLog (LogGain player c NonSupply target) &&&
                      handleAllTriggers GainTrigger
                        ((FromCardEffect card):playerTriggers (playerByName state player) ++ supplyTriggers state)
                        (EffectGain card target)
                        (move c NonSupply target))
                      player
      where
        pile = Map.findWithDefault [] card (nonSupplyPiles state)
        c = head pile

gainTo :: CardDef -> Location -> ActionTemplate
gainTo card target player = gameState' >>= inner
  where
    inner state
      | inSupply state card =
        (addLog (LogGain player top Supply target) &&&
          handleAllTriggers GainTrigger
                            ((FromCardEffect card):playerTriggers (playerByName state player) ++ supplyTriggers state)
                            (EffectGain card target)
                            transferT)
                          player
      | otherwise = noOpSimulation
      where
        top = topOfSupply card state
        transferT player = gameState' >>= \state -> move (topOfSupply card state) Supply target player

gain :: CardDef -> ActionTemplate
gain card player = gainTo card (Discard player) player

reveal :: [Card] -> ActionTemplate
reveal cards player = addLog (LogReveal player cards) player

buy :: CardDef -> ActionTemplate
buy card player =
  gameState' >>= \state ->
      (addLog (LogBuy player card) &&&
        handleAllTriggers BuyTrigger ((FromCardEffect card):playerTriggers (playerByName state player)) (EffectBuy card)
          (gain card))
        player

trash :: Card -> Location -> ActionTemplate
trash card source player =
  gameState' >>= \state ->
      (addLog (LogTrash player card) &&& handleAllTriggers TrashTrigger ((FromCard card source):playerTriggers (playerByName state player)) (EffectTrash card source) transferT)
        player
  where
    transferT _ = modGameState' (transfer card source Trash)

trashAll :: [Card] -> Location -> ActionTemplate
trashAll cards source player =
  gameState' >>= \state ->
      (seqActions (\c -> addLog (LogTrash player c)) cards &&&
        (foldr (\c cont -> handleAllTriggers TrashTrigger ((FromCard c source):playerTriggers (playerByName state player)) (EffectTrash c source) cont)
               transferT
               cards))
        player
  where
    transferT _ = modGameState' $ \state -> foldr (\c s -> transfer c source Trash s) state cards

doDiscard :: Card -> Location -> ActionTemplate
doDiscard card loc player = handleTriggers DiscardTrigger (FromCard card loc) (EffectDiscard card loc) (move card loc (Discard player)) player

discard :: Card -> Location -> ActionTemplate
discard card loc player = (addLog (LogDiscard player [card]) &&& doDiscard card loc) player

-- TODO all triggers have to happen upfront
discardAll :: [Card] -> Location -> ActionTemplate
discardAll cards loc player
  | null cards = noOpSimulation
  | otherwise = (addLog (LogDiscard player cards) &&& seqActions (\c -> doDiscard c loc) cards) player

plusMoney :: Int -> ActionTemplate
plusMoney num _ = modGameState' $ \state -> state { turn = (turn state) { money = num + money (turn state) } }

plusPotion :: ActionTemplate
plusPotion _ = modGameState' $ \state -> state { turn = (turn state) { potions = 1 + potions (turn state) } }

plusCards :: Int -> ActionTemplate
plusCards num player =
  do
    state <- gameState'
    s2 <- SimulationT $ fmap Result (updatePlayerR state player $ \p -> draw p num)
    putGameState' s2
    let newhand = hand (playerByName s2 player)
    let newcards = take (length newhand - length (hand (playerByName state player))) newhand
    addLog (LogDraw (VisibleToPlayer player) player newcards) player

plusBuys :: Int -> ActionTemplate
plusBuys num _ = modGameState' $ \state -> state { turn = (turn state) { buys = num + buys (turn state) } }

plusActions :: Int -> ActionTemplate
plusActions num _ = modGameState' $ \state -> state { turn = (turn state) { actions = num + actions (turn state) } }

plusTokens :: Int -> Token -> ActionTemplate
plusTokens num token player = modGameState' $ \state -> updatePlayer state player $ \p -> p { tokens = Map.insertWith (+) token num (tokens p)}

pass :: ActionTemplate
pass _ = noOpSimulation

goToPhase :: Phase -> ActionTemplate
goToPhase ph _ = modGameState' $ \state -> state { turn = (turn state) { phase = ph } }

-- Macro flows

newTurn :: TurnState
newTurn = TurnState { money = 0, buys = 1, actions = 1, potions = 0, modifiers = Map.empty, turnLog = [], phase = ActionPhase }

nextTurn :: GameState -> SimulationT GameState
nextTurn state = ((discardAll (inPlay player) InPlay
                  &&& discardAll (hand player) (Hand current)
                  &&& plusCards 5)
                  current)
                  >> modGameState' next >> gameState'
  where
    player = activePlayer state
    current = activePlayerId state
    next state = state { turn = newTurn,
                           turnOrder = tail (turnOrder state) ++ [current],
                           ply = 1 + ply state}

checkFinished :: GameState -> Bool
checkFinished state =
  numInSupply state province == 0
  || (Map.member colony (piles state) && numInSupply state colony == 0)
  || Map.size (Map.filter null (piles state)) >= 3

-- TODO island mat should be handled by and end game trigger of sorts
endGame :: GameState -> GameState
endGame state = state { finished = True, players = Map.map clearMats (players state) }
  where
    clearMats player = player { deck = deck player
                                       ++ Map.findWithDefault [] IslandMat (mats player)
                                       ++ Map.findWithDefault [] NativeVillageMat (mats player),
                                mats = Map.delete NativeVillageMat $ Map.delete IslandMat (mats player)
                                }

cleanupPhase :: ActionTemplate
-- TODO this should probably have some sort of 'andThen' construct or simply separate monads ...
cleanupPhase _ = gameState' >>= nextTurn >>= \state -> if checkFinished state then modGameState' endGame else noOpSimulation

buyPhase :: ActionTemplate
buyPhase name = gameState' >>= inner
  where
    inner state
      | buys (turn state) == 0 = noOpSimulation
      | otherwise = optDecision (ChooseCard (EffectBuy unknownDef)
                                                        candidates
                                                        (\card -> (plusBuys (-1) &&& payCosts (cost state (typ card)) &&& buy (typ card) &&& buyPhase) name))
                                                      name
      where
        payCosts cost _ = modGameState' (\state ->
                            let t = turn state
                            in state { turn = t { money = money t - moneyCost cost, potions = potions t - potionCost cost }})
                            >> noOpSimulation
        moneyToSpend = money (turn state)
        potionToSpend = potions (turn state)
        candidates = map (`topOfSupply` state) $ filter (`canBuy` state) $ affordableCards (fullCost moneyToSpend potionToSpend) state

playTreasures :: ActionTemplate
playTreasures name = gameState' >>= inner
  where
    inner state
      | length treasures > 0 = playTreasureDecision
      | otherwise = noOpSimulation
      where
        treasures = filter isTreasure (hand (activePlayer state))
        playTreasureDecision = optDecision (ChooseCards (EffectPlayTreasure unknown)
                                                 treasures
                                                 (0,length treasures)
                                                 -- we cycle play treasures because cards like IGG might add new ones
                                                 (\cards -> playAll cards name >> playTreasures name))
                                name


useCoinTokens :: ActionTemplate
useCoinTokens player = do
    state <- gameState'
    let numCoins = Map.findWithDefault 0 CoinToken (tokens $ playerByName state player)
    simWhen (numCoins > 0) $
        optDecision (ChooseNumber (EffectUseTokens CoinToken) (0,numCoins) cont) player
    where
        cont num = ((plusMoney num &&& plusTokens (- num) CoinToken)) player

actionPhase :: ActionTemplate
actionPhase name = do
    state <- gameState'
    let actionsInHand = filter isAction (hand (activePlayer state))
    simWhen (not (actions (turn state) == 0 || null actionsInHand)) $
        optDecision (ChooseCard (EffectPlayAction unknown) actionsInHand (\card -> (plusActions (-1) &&& play card &&& actionPhase) name)) name

startOfTurn :: ActionTemplate
startOfTurn player = gameState' >>= inner
  where
    inner state
        | null durations = noOpSimulation
        | otherwise = putGameState' (updatePlayer state player (const p { inPlayDuration = [], inPlay = durationCards ++ inPlay p }))
            >> allTriggers player
        where
          allTriggers = handleAllTriggers StartOfTurnTrigger (map triggerSource durations) NullEffect pass
          p = playerByName state player
          durations = inPlayDuration p
          durationCards = Either.lefts durations
          triggerSource (Left card) = FromCard card InPlay
          triggerSource (Right def) = FromCardEffect def

playTurn :: ActionTemplate
playTurn =
  ((\name -> gameState' >>= \state -> addLog (LogTurn name (turnNo state) state) name) &&&
   goToPhase ActionPhase &&& startOfTurn &&& actionPhase &&&
   goToPhase BuyPhase &&& useCoinTokens &&& playTreasures &&& buyPhase &&&
   goToPhase CleanupPhase &&& cleanupPhase)

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

seedSim :: StdGen -> SimulationState
seedSim = SimulationState nullState

evalGameT :: GameT a -> SimulationState -> a
evalGameT g st = St.evalState g st

evalSim :: SimulationT a -> SimulationState -> MaybeDecision a (SimulationT a)
evalSim sim st = St.evalState (nextStep sim) st

runSim :: SimulationT a -> SimulationState -> (MaybeDecision a (SimulationT a),SimulationState)
runSim sim st = (res,st')
  where
    (res,st') = St.runState (nextStep sim) st

sim2Gen :: SimulationState -> StdGen
sim2Gen = randomGenerator

