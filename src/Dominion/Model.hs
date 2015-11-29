{-# LANGUAGE TupleSections #-}
module Dominion.Model where

import qualified Control.Monad as M
import qualified Control.Monad.Trans.State.Lazy as St
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
    deriving (Eq, Read, Show)

data Location = Hand String | Discard String | TopOfDeck String | InPlay | Trash | Supply
  deriving (Eq, Show)

data Edition = Base | Intrigue | Seaside | Alchemy | Prosperity | Cornucopia | Hinterlands | DarkAges | Guilds | Adventures | Promo
  deriving (Eq, Read, Show)


-- The fundamental domain types

-- TODO this is really a card type, rather than a concrete card sitting somewhere ...
-- there is design potential to include the concept of a concrete card in the model
-- it would have an identity, and a location (and could be traced throughout the game)
-- > it allows cards to be transferred without keeping location as a separate concept
-- > it allows precise implementation of concepts like, 'if X then do Y to this card'

data Card = Card { -- Card id is purely a performance artifact to avoid string comparisons
                   cardId :: !Int,
                   cardName :: !String,
                   edition :: !Edition,
                   -- TODO this only works for Base
                   -- breaks for cards like peddlar having state dependent cost
                   -- breaks for potion cost
                   cost :: !Int,
                   types :: ![CardType],
                   cardPoints :: !(Player -> Int),
                   onPlay :: !Action
                   }

instance Show Card where
  show = cardName

instance Eq Card where
  (==) c1 c2 = cardId c1 == cardId c2

instance Ord Card where
  c1 <= c2 = cardId c1 <= cardId c2

-- Card definition helpers

noPoints = const 0

treasure id name cost money edition = Card id name edition cost [Treasure] noPoints (plusMoney money)
action id name cost effect edition = Card id name edition cost [Action] noPoints effect
attack id name cost effect edition = Card id name edition cost [Action, Attack] noPoints effect
victory id name cost points edition = Card id name edition cost [Victory] (const points) pass
carddef id name cost types points effect edition = Card id name edition cost types points effect

-- Basic cards

copper   = treasure 0 "Copper" 0 1 Base
silver   = treasure 1 "Silver" 3 2 Base
gold     = treasure 2 "Gold" 6 3 Base
estate   = victory 3 "Estate" 2 1 Base
duchy    = victory 4 "Duchy" 5 3 Base
province = victory 5 "Province" 8 6 Base
curse    = carddef 6 "Curse" 0 [CurseType] (const (-1)) pass Base
-- TODO Potion obviously does not actually do anything here yet
potion   = treasure 7 "Potion" 4 0 Alchemy
platinum = treasure 8 "Platinum" 9 5 Prosperity
colony   = victory 9 "Colony" 11 10 Prosperity

basicCards = [copper, silver, gold, estate, duchy, province, curse, potion, platinum, colony]



type PlayerId = String

data Player = Player { name :: PlayerId,
                       hand :: [Card],
                       inPlay :: [Card],
                       deck :: [Card],
                       discardPile :: [Card]
                       }
                       deriving (Eq, Show)

data TurnState = TurnState { money :: Int,
                             buys :: Int,
                             actions :: Int
                             }
                             deriving (Show)


data GameType = StandardGame | ColonyGame deriving (Eq, Read, Show)

data GameState = GameState { players :: Map.Map PlayerId Player,
                             turnOrder :: [PlayerId],
                             trashPile :: [Card],
                             turn :: TurnState,
                             piles :: Map.Map Card Int,
                             ply :: Int
                             }

data Visibility = AllPlayers | VisibleToPlayer PlayerId

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

-- Decision & decision type are not rich enough for smart bots
-- they need to be able to known more about what the decision really is
-- What drives a decision:
-- > the current state
-- > who decides
-- > who & what triggered the decision (card, effect)
-- > what is decided (the crux)
--   > what action
--   > what target

data DecisionType = QPlay | QBuy | QDraw | QTreasures | QTrash | QGain | QDiscard Location | QOption

data Decision = YesNo DecisionType Card (Bool -> Simulation) |
                Choice DecisionType [Card] (Card -> Simulation) |
                Choices DecisionType [Card] (Int,Int) ([Card] -> Simulation) |
                Optional Decision Simulation

data GameStep = State GameState | Decision PlayerId GameState Decision

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
    (summarizeCards $ concatMap (\(c,n) -> replicate n c) (Map.toList (piles g))) ++
    " ]\n" ++
    "  turn: { " ++
    "actions: " ++ show (actions (turn g)) ++ ", buys: " ++ show (buys (turn g)) ++ ", money: " ++ show (money (turn g)) ++
    " }\n" ++
    "}"


-- Game creation

mkPlayer :: String -> SimulationT Player
mkPlayer name = draw Player { name = name, hand = [], discardPile = initialDeck, deck = [], inPlay = [] } 5

initialDeck :: [Card]
initialDeck = replicate 7 copper ++ replicate 3 estate

isStandardVictory c
  | c == estate = True
  | c == duchy = True
  | c == province = True
  | c == colony = True
  | otherwise = False

mkGame :: GameType -> [String] -> [Card] -> SimulationT GameState
mkGame typ names kingdomCards =
  (sequence $ map mkPlayer names) >>= \players ->
    return $
    GameState { players = Map.fromList $ zip names players,
                turnOrder = names,
                piles = Map.fromList $ map (\c -> (c, noInPile c)) (standardCards ++ kingdomCards),
                trashPile = [],
                turn = newTurn,
                ply = 1
                }
  where
    standardCards = extraCards ++ [estate,duchy,province,copper,silver,gold,curse]
    extraCards = if typ == ColonyGame then [platinum, colony] else []
    playerNo = length names
    noInPile card
      | isStandardVictory card && playerNo == 2 = 8
      | isStandardVictory card = 12
      | card == curse && playerNo == 2 = 10
      | card == curse && playerNo == 3 = 20
      | card == curse = 30
      | card == gold = 30
      | card == silver = 40
      | card == copper = 60 - 7 * playerNo
      | otherwise = 10



-- Combinators

decision :: Decision -> Action
decision decision player state = return $ Decision player state decision

optDecision :: Decision -> Action
optDecision decision player state = return $ Decision player state (Optional decision (return (State state)))

andThenI :: Decision -> (GameState -> Simulation) -> Decision
andThenI (YesNo caption card cont) f = YesNo caption card (\b -> cont b `andThen` f)
andThenI (Choice caption cards cont) f = Choice caption cards (\c -> cont c `andThen` f)
andThenI (Choices caption cards lohi cont) f = Choices caption cards lohi (\cs -> cont cs `andThen` f)
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

yesNo :: DecisionType -> Card -> (Bool -> Action) -> Action
yesNo typ card cont player state =
  decision (YesNo typ card (\bool -> cont bool player state)) player state

chooseOne :: DecisionType -> [Card] -> (Card -> Action) -> Action
chooseOne typ choices cont player state =
  decision (Choice typ choices (\card -> cont card player state)) player state

chooseMany :: DecisionType -> [Card] -> (Int,Int) -> ([Card] -> Action) -> Action
chooseMany typ choices lohi cont player state =
  decision (Choices typ choices lohi (\chosen -> cont chosen player state)) player state


-- Game functions

moveTo :: Card -> Location -> GameState -> GameState
moveTo c (Hand player) state      = updatePlayer state player (\p -> p { hand = c : hand p })
moveTo c (Discard player) state   = updatePlayer state player (\p -> p { discardPile = c : discardPile p })
moveTo c (TopOfDeck player) state = updatePlayer state player (\p -> p { deck = c : deck p })
moveTo c InPlay state             = updatePlayer state (name (activePlayer state)) (\p -> p { inPlay = c : inPlay p })
moveTo c Trash state              = state { trashPile = c : trashPile state }
moveTo c Supply state             = state { piles = Map.adjust (+1) c (piles state) }

moveFrom :: Card -> Location -> GameState -> GameState
moveFrom c (Hand player) state      = updatePlayer state player (\p -> p { hand = L.delete c $ hand p })
moveFrom c (Discard player) state   = updatePlayer state player (\p -> p { discardPile = L.delete c $ discardPile p })
moveFrom c (TopOfDeck player) state = updatePlayer state player (\p -> p { deck = L.delete c $ deck p })
moveFrom c InPlay state             = updatePlayer state (name (activePlayer state)) (\p -> p { inPlay = L.delete c $ inPlay p })
moveFrom c Trash state              = state { trashPile = L.delete c $ trashPile state }
moveFrom c Supply state             = state { piles = Map.adjust (+(-1)) c (piles state) }

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


availableCards :: GameState -> [Card]
availableCards g = Map.keys $ Map.filter (>0) (piles g)

supply :: GameState -> [(Card, Int)]
supply state = Map.toList $ piles state

inSupply :: GameState -> Card -> Bool
inSupply state card = maybe False (>0) (Map.lookup card (piles state))

numInSupply :: GameState -> Card -> Int
numInSupply state card = maybe 0 id (Map.lookup card (piles state))

hasCardType :: Card -> CardType -> Bool
hasCardType card typ = typ `elem` types card

isAction card = hasCardType card Action
isReaction card = hasCardType card Reaction
isTreasure card = hasCardType card Treasure
isAttack card = hasCardType card Attack


allCards :: Player -> [Card]
allCards s = concatMap (\f -> f s) [hand, inPlay, discardPile, deck]

points :: Player -> Int
points s = sum $ map (`cardPoints` s) $ allCards s

turnNo :: GameState -> Int
turnNo g = ((ply g + 1) `div` length (players g))

unknown = Card (-1) "Unknown" Base 0 [] (\_ -> 0) pass

-- Removes invisble information from the state such as opponents hands
-- It assumes some intelligent information retention such as about own deck content
-- but more could be done for opponents
visibleState :: PlayerId -> GameState -> GameState
-- TODO figure out a more efficient implementation that still gives access to a players full deck
visibleState id state = state -- state { players = Map.map anonymize (players state) }
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

playEffect :: Card -> Action
playEffect card player state = info AllPlayers (player ++ " plays " ++ cardName card) >> onPlay card player state

play :: Card -> Action
play card player state = playEffect card player (transfer card (Hand player) InPlay state)

playAll :: [Card] -> Action
playAll [] _ state = toSimulation state
playAll (c:cs) player state = play c player state `andThen` playAll cs player

gainFrom :: Card -> Location -> Action
gainFrom card source player state =
  info AllPlayers (player ++ " gains " ++ cardName card) >> return (State $ transfer card source (Discard player) state)


gainTo :: Card -> Location -> Action
gainTo card target player state
  | inSupply state card =
    info AllPlayers (player ++ " gains " ++ cardName card) >> return (State $ transfer card Supply target state)
  | otherwise = toSimulation state

gain :: Card -> Action
gain card player = gainTo card (Discard player) player

buy :: Card -> Action
buy card player state =
  info AllPlayers (player ++ " buys " ++ cardName card) >> return (State $ transfer card Supply (Discard player) state)

trash :: Card -> Location -> Action
trash card source player state =
  info AllPlayers (player ++ " trashes " ++ cardName card) >> return (State $ transfer card source Trash state)

discard :: Card -> Location -> Action
discard card loc player state =
  info AllPlayers (player ++ " discards " ++ cardName card) >> return (State $ transfer card loc (Discard player) state)

plusMoney :: Int -> Action
plusMoney num _ state = toSimulation $ state { turn = (turn state) { money = num + money (turn state) } }

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

pass :: Action
pass _ = toSimulation

-- Macro flows

newTurn :: TurnState
newTurn = TurnState { money = 0, buys = 1, actions = 1 }

cleanupPhase :: Action
cleanupPhase _ state = M.liftM State nnext
  where
    current = head (turnOrder state)
    next = state { turn = newTurn,
                   turnOrder = tail (turnOrder state) ++ [current],
                   ply = 1 + ply state}

    -- TODO can cleanup trigger anything like discard ?
    nnext = updatePlayerR next current (\p -> draw p { hand = [], inPlay = [], discardPile = hand p ++ inPlay p ++ discardPile p } 5)

buyPhase :: Action
buyPhase name state
  | length treasures > 0 = playTreasureDecision
  | buys (turn state) == 0 = toSimulation state
  | otherwise = buyDecision state
  where
    treasures = filter isTreasure (hand (activePlayer state))

    playTreasureDecision = optDecision (Choices QTreasures
                                             treasures
                                             (0,length treasures)
                                             (\cards -> playAll cards name state))
                            name state
      `andThen` buyDecision

    buyDecision s2 = optDecision (Choice QBuy
                                      affordableCards
                                      (\card -> (plusBuys (-1) &&& plusMoney (- (cost card)) &&& buy card &&& buyPhase) name s2))
                                    name s2
      where
        moneyToSpend = money (turn s2)
        affordableCards = filter ((<=moneyToSpend) . cost) $ availableCards s2


actionPhase :: Action
actionPhase name state
  | actions (turn state) == 0 || null actionsInHand = toSimulation state
  | otherwise = optDecision (Choice QPlay actionsInHand (\card -> (plusActions (-1) &&& play card &&& actionPhase) name state))
                         name state
  where
    actionsInHand = filter isAction (hand (activePlayer state))

playTurn :: Action
playTurn name state =
  info (VisibleToPlayer name) ("Your turn:\n" ++ show (visibleState name state))
  >> (actionPhase &&& buyPhase &&& cleanupPhase) name state

finished :: GameState -> Bool
finished state =
  numInSupply state province == 0
  || Map.size (Map.filter (==0) (piles state)) >= 3


-- Simulation Stepping

evalSim :: SimulationT a -> StdGen -> a
evalSim sim gen = St.evalState sim (gen,[])

runSim :: SimulationT a -> StdGen -> (a,SimulationState)
runSim sim gen = (res,(resgen,reverse infos))
  where
    (res,(resgen,infos)) = St.runState sim (gen,[])
