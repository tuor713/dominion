{-# LANGUAGE TupleSections #-}
module Dominion.Model where

import qualified Control.Monad.Random as MRandom
import qualified Control.Monad.Trans.State.Lazy as St
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Map.Lazy as LMap
import qualified Data.Maybe as Maybe
import System.Random (StdGen, mkStdGen, randomR, newStdGen)


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

type RandomState a = St.State StdGen a


-- The fundamental domain types

-- TODO this is really a card type, rather than a concrete card sitting somewhere ...
-- there is design potential to include the concept of a concrete card in the model
-- it would have an identity, and a location (and could be traced throughout the game)
-- > it allows cards to be transferred without keeping location as a separate concept
-- > it allows precise implementation of concepts like, 'if X then do Y to this card'
data Card = Card { cardId :: Int,
                   cardName :: String,
                   edition :: Edition,
                   -- TODO this only works for Base
                   -- breaks for cards like peddlar having state dependent cost
                   -- breaks for potion cost
                   cost :: Int,
                   types :: [CardType],
                   cardPoints :: Player -> Int,
                   onPlay :: PlayerId -> GameState -> GameStep
                   }

instance Show Card where
  show = cardName

instance Eq Card where
  (==) c1 c2 = cardId c1 == cardId c2

instance Ord Card where
  c1 <= c2 = show c1 <= show c2

type PlayerId = String
allPlayerId = "::all::"

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

data GameState = GameState { players :: Map.Map PlayerId Player,
                             turnOrder :: [PlayerId],
                             trashPile :: [Card],
                             turn :: TurnState,
                             piles :: Map.Map Card Int,
                             ply :: Int,
                             -- TODO Is there a better to do this?
                             gen :: StdGen
                             }

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

data Decision = YesNo DecisionType Card (Bool -> GameStep) |
                Choice DecisionType [Card] (Card -> GameStep) |
                Choices DecisionType [Card] (Int,Int) ([Card] -> GameStep) |
                Optional Decision GameStep

data Interaction = Decision Decision | Info String GameStep

data GameStep = State GameState | Interaction PlayerId GameState Interaction

type Action = PlayerId -> GameState -> GameStep


instance Show GameState where
  show g = "Game {\n" ++
    "  turn: " ++ (show (turnNo g)) ++ "\n" ++
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


-- Combinators

info :: String -> Action
info msg to state = Interaction to state $ Info msg (State state)

decision :: Decision -> Action
decision decision player state = Interaction player state (Decision decision)

optDecision :: Decision -> Action
optDecision decision player state = Interaction player state (Decision (Optional decision (State state)))

andThenI :: Decision -> (GameState -> GameStep) -> Decision
andThenI (YesNo caption card cont) f = YesNo caption card (\b -> cont b `andThen` f)
andThenI (Choice caption cards cont) f = Choice caption cards (\c -> cont c `andThen` f)
andThenI (Choices caption cards lohi cont) f = Choices caption cards lohi (\cs -> cont cs `andThen` f)
andThenI (Optional inner fallback) f = Optional (inner `andThenI` f) (fallback `andThen` f)

andThen :: GameStep -> (GameState -> GameStep) -> GameStep
andThen (State state) f = f state
andThen (Interaction player state (Info msg next)) f = Interaction player state (Info msg (next `andThen` f))
andThen (Interaction player state (Decision choice)) f = decision (choice `andThenI` f) player state

toAction :: GameStep -> Action
toAction step _ _ = step

app :: PlayerId -> GameState -> Action -> GameStep
app p s action = action p s

{-# INLINE (&&&) #-}
(&&&) :: Action -> Action -> Action
(&&&) act1 act2 player state = act1 player state `andThen` act2 player

{-# INLINE (&&+) #-}
(&&+) :: Action -> (PlayerId -> GameState -> Action) -> Action
(&&+) act1 act2 player state = act1 player state `andThen` (\s2 -> app player s2 $ act2 player s2)

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
moveTo c (Hand player) state = updatePlayer state player (\p -> p { hand = c : hand p })
moveTo c (Discard player) state = updatePlayer state player (\p -> p { discardPile = c : discardPile p })
moveTo c (TopOfDeck player) state = updatePlayer state player (\p -> p { deck = c : deck p })
moveTo c InPlay state = updatePlayer state (name (activePlayer state)) (\p -> p { inPlay = c : inPlay p })
moveTo c Trash state = state { trashPile = c : trashPile state }
moveTo c Supply state = error "Not implemented: moving card to supply"

moveFrom :: Card -> Location -> GameState -> GameState
moveFrom c (Hand player) state      = updatePlayer state player (\p -> p { hand = L.delete c $ hand p })
moveFrom c (Discard player) state   = updatePlayer state player (\p -> p { discardPile = L.delete c $ discardPile p })
moveFrom c (TopOfDeck player) state = updatePlayer state player (\p -> p { deck = L.delete c $ deck p })
moveFrom c InPlay state             = updatePlayer state (name (activePlayer state)) (\p -> p { inPlay = L.delete c $ inPlay p })
moveFrom c Trash state              = state { trashPile = L.delete c $ trashPile state }
moveFrom c Supply state             = state { piles = Map.adjust (+(-1)) c (piles state) }
  where
    takeOne (card,num) = (card,if card == c then num -1 else num)

transfer :: Card -> Location -> Location -> GameState -> GameState
transfer c from to state = moveTo c to (moveFrom c from state)

updatePlayer :: GameState -> String -> (Player -> Player) -> GameState
updatePlayer state pname f = state { players = Map.adjust f pname (players state) }

updatePlayerR :: GameState -> String -> (Player -> RandomState Player) -> GameState
updatePlayerR state pname f =
  state { players = Map.insert pname new (players state), gen = resultgen }
  where
    prev = (players state) Map.! pname
    (new, resultgen) = St.runState (f prev) (gen state)


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

shuffle :: [a] -> RandomState [a]
shuffle xs = St.state $ \gen -> shuffle' xs gen

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

reshuffleDiscard :: Player -> RandomState Player
reshuffleDiscard p =
  do
    shuffled <- shuffle (discardPile p)
    return (p { discardPile = [], deck = deck p ++ shuffled })

canDraw :: Player -> Bool
canDraw p = not (null (hand p)) || not (null (discardPile p))

draw :: Player -> Int -> RandomState Player
draw p num
  | length (deck p) < num = fmap (\p -> p !!! num) (reshuffleDiscard p)
  | otherwise = return $ p !!! num
  where
    (!!!) p num = p { hand = take num (deck p) ++ hand p, deck = drop num (deck p)}

ensureCanDraw :: Int -> GameState -> PlayerId -> Maybe GameState
ensureCanDraw num state name
  | length (deck p) >= num = Just state
  | length (deck p2) >= num = Just s2
  | otherwise = Nothing
  where
    p = playerByName state name
    s2 = updatePlayerR state name reshuffleDiscard
    p2 = playerByName s2 name

-- Game action

playEffect :: Card -> Action
playEffect card player state =
  info (player ++ " plays " ++ cardName card) allPlayerId state `andThen` onPlay card player

play :: Card -> Action
play card player state = playEffect card player (transfer card (Hand player) InPlay state)

playAll :: [Card] -> Action
playAll [] _ state = State $ state
playAll (c:cs) player state = play c player state  `andThen` playAll cs player

gainFrom :: Card -> Location -> Action
gainFrom card source player state =
  info (player ++ " gains " ++ cardName card) allPlayerId $ transfer card source (Discard player) state


gainTo :: Card -> Location -> Action
gainTo card target player state
  | inSupply state card =
    info (player ++ " gains " ++ cardName card) allPlayerId $ transfer card Supply target state
  | otherwise = State state

gain :: Card -> Action
gain card player = gainTo card (Discard player) player

buy :: Card -> Action
buy card player state =
  info (player ++ " buys " ++ cardName card) allPlayerId $ transfer card Supply (Discard player) state

trash :: Card -> Location -> Action
trash card source player state =
  info (player ++ " trashes " ++ cardName card) allPlayerId $ transfer card source Trash state

discard :: Card -> Location -> Action
discard card loc player state =
  info (player ++ " discards " ++ cardName card) allPlayerId $ transfer card loc (Discard player) state

plusMoney :: Int -> Action
plusMoney num _ state = State $ state { turn = (turn state) { money = num + money (turn state) } }

plusCards :: Int -> Action
plusCards num player state = info ("Drew " ++ summarizeCards newcards) player s2
  where
    s2 = updatePlayerR state player $ \p -> draw p num
    newhand = hand (playerByName s2 player)
    newcards = take (length newhand - length (hand (playerByName state player))) newhand

plusBuys :: Int -> Action
plusBuys num _ state = State $ state { turn = (turn state) { buys = num + buys (turn state) } }

plusActions :: Int -> Action
plusActions num _ state = State $ state { turn = (turn state) { actions = num + actions (turn state) } }

pass :: Action
pass _ state = State state

-- Macro flows

newTurn :: TurnState
newTurn = TurnState { money = 0, buys = 1, actions = 1 }

cleanupPhase :: Action
cleanupPhase _ state = State nnext
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
  | buys (turn state) == 0 = State state
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
  | actions (turn state) == 0 || null actionsInHand = State state
  | otherwise = optDecision (Choice QPlay actionsInHand (\card -> (plusActions (-1) &&& play card &&& actionPhase) name state))
                         name state
  where
    actionsInHand = filter isAction (hand (activePlayer state))

playTurn :: Action
playTurn name state =
  (info ("Your turn:\n" ++ show (visibleState name state)) &&& actionPhase &&& buyPhase &&& cleanupPhase) name state

finished :: GameState -> Bool
finished state =
  numInSupply state Card { cardId = 5, cardName = "Province" } == 0
  || Map.size (Map.filter (==0) (piles state)) >= 3
