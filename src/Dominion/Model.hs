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
data Card = Card { cardName :: String,
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
  (==) c1 c2 = cardName c1 == cardName c2

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

data GameState = GameState { players :: [Player],
                             trashPile :: [Card],
                             turn :: TurnState,
                             piles :: Map.Map Card Int,
                             ply :: Int,
                             -- TODO Is there a better to do this?
                             gen :: StdGen
                             }

data DecisionType = QPlay | QBuy | QDraw | QTreasures | QTrash | QGain | QDiscard |
  QOption Card

data Decision = YesNo DecisionType (Bool -> GameStep) |
                Choice DecisionType [Card] (Card -> GameStep) |
                Choices DecisionType [Card] ([Card] -> Bool) ([Card] -> GameStep) |
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
andThenI (YesNo caption cont) f = YesNo caption (\b -> cont b `andThen` f)
andThenI (Choice caption cards cont) f = Choice caption cards (\c -> cont c `andThen` f)
andThenI (Choices caption cards validator cont) f = Choices caption cards validator (\cs -> cont cs `andThen` f)
andThenI (Optional inner fallback) f = Optional (inner `andThenI` f) (fallback `andThen` f)

andThen :: GameStep -> (GameState -> GameStep) -> GameStep
andThen (State state) f = f state
andThen (Interaction player state (Info msg next)) f = Interaction player state (Info msg (next `andThen` f))
andThen (Interaction player state (Decision choice)) f = decision (choice `andThenI` f) player state

toAction :: GameStep -> Action
toAction step _ _ = step

app :: PlayerId -> GameState -> Action -> GameStep
app p s action = action p s

(&&&) :: Action -> Action -> Action
(&&&) act1 act2 player state = act1 player state `andThen` act2 player

(&&+) :: Action -> (PlayerId -> GameState -> Action) -> Action
(&&+) act1 act2 player state = act1 player state `andThen` (\s2 -> app player s2 $ act2 player s2)

(&&=) :: (a -> Action) -> a -> Action
(&&=) = ($)

yesNo :: DecisionType -> (Bool -> Action) -> Action
yesNo typ cont player state =
  decision (YesNo typ (\bool -> cont bool player state)) player state

chooseOne :: DecisionType -> [Card] -> (Card -> Action) -> Action
chooseOne typ choices cont player state =
  decision (Choice typ choices (\card -> cont card player state)) player state

chooseMany :: DecisionType -> [Card] -> ([Card] -> Bool) -> ([Card] -> Action) -> Action
chooseMany typ choices val cont player state =
  decision (Choices typ choices val (\chosen -> cont chosen player state)) player state


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
updatePlayer state pname f =
  state { players = map (\p -> if name p == pname then f p else p) (players state) }

updatePlayerR :: GameState -> String -> (Player -> RandomState Player) -> GameState
updatePlayerR state pname f =
  state { players = ps, gen = resultgen }
  where
    (ps, resultgen) = St.runState (sequence $ map (\p -> if name p == pname then f p else return p) (players state)) (gen state)


-- Queries and predicates

activePlayer :: GameState -> Player
activePlayer state = head $ players state

activePlayerId :: GameState -> PlayerId
activePlayerId = name . activePlayer

playerByName :: GameState -> PlayerId -> Player
playerByName state pname = head $ filter ((== pname) . name) (players state)

playerNames :: GameState -> [String]
playerNames state = map name (players state)

opponentNames :: GameState -> String -> [String]
opponentNames state player = map name $ opponents state player

opponents :: GameState -> String -> [Player]
opponents state player = filter ((/= player) . name) $ players state


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

unknown = Card "Unknown" Base 0 [] (\_ -> 0) pass

-- Removes invisble information from the state such as opponents hands
-- It assumes some intelligent information retention such as about own deck content
-- but more could be done for opponents
visibleState :: PlayerId -> GameState -> GameState
visibleState id state = state { players = map anonymize (players state)}
  where
    anonymize p
      | id == name p = p { deck = L.sort (deck p) }
      | otherwise = p { hand = replicate (length (hand p)) unknown, deck = replicate (length (deck p)) unknown }


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
cleanupPhase _ state = State $ state { turn = newTurn, players = tail (players state) ++ [current''], ply = 1 + ply state, gen = gen'}
  where
    current = head (players state)
    -- TODO can cleanup trigger anything like discard ?
    current' = current { hand = [], inPlay = [], discardPile = hand current ++ inPlay current ++ discardPile current }
    (current'',gen') = St.runState (draw current' 5) (gen state)

buyPhase :: Action
buyPhase name state
  | length treasures > 0 = playTreasureDecision
  | buys (turn state) == 0 = State state
  | otherwise = buyDecision state
  where
    treasures = filter isTreasure (hand (activePlayer state))

    playTreasureDecision = optDecision (Choices QTreasures
                                             treasures
                                             (const True)
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
  numInSupply state Card { cardName = "Province" } == 0
  || Map.size (Map.filter (==0) (piles state)) >= 3
