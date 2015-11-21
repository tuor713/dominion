{-# LANGUAGE TupleSections #-}
module Dominion.Model where

import qualified Control.Monad.Trans.State.Lazy as St
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import System.Random (StdGen, mkStdGen, randomR, newStdGen)


data CardType = Action | Treasure | Victory | CurseType {- This is a bit of a hack -} |
    Reaction | Attack | Duration |
    Prize | Looter | Ruins | Knight | Reserve | Traveller
    deriving (Eq, Read, Show)

data Location = Hand String | Discard String | TopOfDeck String | InPlay | Trash | Supply
  deriving (Eq, Show)

data Edition = Base | Intrigue | Seaside | Alchemy | Prosperity | Cornucopia | Hinterlands | DarkAges | Guilds
  deriving (Eq, Read, Show)

type RandomState a = St.State StdGen a


-- The fundamental domain types

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
                             piles :: [[Card]],
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


instance Show GameState where
  show g = "Game {\n" ++
    "  turn: " ++ (show ((ply g + 1) `div` length (players g))) ++ "\n" ++
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
    (summarizeCards $ concat (piles g)) ++
    " ]\n" ++
    "  turn: { " ++
    "actions: " ++ show (actions (turn g)) ++ ", buys: " ++ show (buys (turn g)) ++ ", money: " ++ show (money (turn g)) ++
    " }\n" ++
    "}"


-- Combinators

info :: PlayerId -> String -> GameState -> GameStep
info to msg state = Interaction to state $ Info msg (State state)

decision :: PlayerId -> Decision -> Bool -> GameState -> GameStep
decision player decision False state = Interaction player state (Decision decision)
decision player decision True state = Interaction player state (Decision (Optional decision (State state)))

andThenI :: Decision -> (GameState -> GameStep) -> Decision
andThenI (YesNo caption cont) f = YesNo caption (\b -> cont b `andThen` f)
andThenI (Choice caption cards cont) f = Choice caption cards (\c -> cont c `andThen` f)
andThenI (Choices caption cards validator cont) f = Choices caption cards validator (\cs -> cont cs `andThen` f)
andThenI (Optional inner fallback) f = Optional (inner `andThenI` f) (fallback `andThen` f)

andThen :: GameStep -> (GameState -> GameStep) -> GameStep
andThen (State state) f = f state
andThen (Interaction player state (Info msg next)) f = Interaction player state (Info msg (next `andThen` f))
andThen (Interaction player state (Decision choice)) f = decision player (choice `andThenI` f) False state

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
moveFrom c Supply state             = state { piles = map takeFirst (piles state) }
  where
    takeFirst [] = []
    takeFirst (x:xs) = if x == c then xs else x:xs

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
availableCards g = concatMap (take 1) (piles g)

inSupply :: GameState -> Card -> Bool
inSupply state card = card `elem` availableCards state

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

shuffle :: [a] -> RandomState [a]
shuffle ys = St.state $ \gen -> shuffle' gen ys []
  where
    shuffle' g [] acc = (acc,g)
    shuffle' g l acc = shuffle' gnew (lead ++ xs) (x:acc)
      where
        (k,gnew) = randomR (0, length l - 1) g
        (lead, x:xs) = splitAt k l

-- Clojure / F# style threading
(|>) :: a -> (a -> b) -> b
(|>) x f = f x

summarizeCards :: [Card] -> String
summarizeCards cards =
  cards |>
  L.sort |>
  L.group |>
  L.map (\cs -> (if (length cs > 1) then show (length cs) ++ "x " else "") ++ show (head cs)) |>
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

type Action = PlayerId -> GameState -> GameStep

playEffect :: Card -> PlayerId -> GameState -> GameStep
playEffect card player state =
  info allPlayerId (player ++ " plays " ++ cardName card) state `andThen` onPlay card player

play :: Card -> PlayerId -> GameState -> GameStep
play card player state = playEffect card player (transfer card (Hand player) InPlay state)

playAll :: [Card] -> PlayerId -> GameState -> GameStep
playAll [] _ state = State $ state
playAll (c:cs) player state = play c player state  `andThen` playAll cs player

gainFrom :: Card -> Location -> Action
gainFrom card source player state =
  info allPlayerId (player ++ " gains " ++ cardName card) $ transfer card source (Discard player) state


gainTo :: Card -> Location -> Action
gainTo card target player state
  | inSupply state card =
    info allPlayerId (player ++ " gains " ++ cardName card) $ transfer card Supply target state
  | otherwise = State state

gain :: Card -> Action
gain card player = gainTo card (Discard player) player

buy :: Card -> PlayerId -> GameState -> GameStep
buy card player state =
  info allPlayerId (player ++ " buys " ++ cardName card)
  $ transfer card Supply (Discard player) state

trash :: Card -> Location -> PlayerId -> GameState -> GameStep
trash card source player state =
  info allPlayerId (player ++ " trashes " ++ cardName card)
  $ transfer card source Trash state

discard :: Card -> Location -> PlayerId -> GameState -> GameStep
discard card loc player state =
  info allPlayerId (player ++ " discards " ++ cardName card)
  $ transfer card loc (Discard player) state

plusMoney :: Int -> PlayerId -> GameState -> GameStep
plusMoney num _ state = State $ state { turn = (turn state) { money = num + money (turn state) } }

plusCards :: Int -> PlayerId -> GameState -> GameStep
plusCards num player state = info player ("Drew " ++ summarizeCards newcards) s2
  where
    s2 = updatePlayerR state player $ \p -> draw p num
    newhand = hand (playerByName s2 player)
    newcards = take (length newhand - length (hand (playerByName state player))) newhand

plusBuys :: Int -> PlayerId -> GameState -> GameStep
plusBuys num _ state = State $ state { turn = (turn state) { buys = num + buys (turn state) } }

plusActions :: Int -> PlayerId -> GameState -> GameStep
plusActions num _ state = State $ state { turn = (turn state) { actions = num + actions (turn state) } }

pass :: PlayerId -> GameState -> GameStep
pass _ state = State state

(&) :: (PlayerId -> GameState -> GameStep) -> (PlayerId -> GameState -> GameStep) -> PlayerId -> GameState -> GameStep
(&) act1 act2 player state = act1 player state `andThen` act2 player


-- Macro flows

newTurn :: TurnState
newTurn = TurnState { money = 0, buys = 1, actions = 1 }

cleanupPhase :: GameState -> GameStep
cleanupPhase state = State $ state { turn = newTurn, players = tail (players state) ++ [current''], ply = 1 + ply state, gen = gen'}
  where
    current = head (players state)
    -- TODO can cleanup trigger anything like discard ?
    current' = current { hand = [], inPlay = [], discardPile = hand current ++ inPlay current ++ discardPile current }
    (current'',gen') = St.runState (draw current' 5) (gen state)

buyPhase :: GameState -> GameStep
buyPhase state
  | length treasures > 0 = playTreasureDecision
  | buys (turn state) == 0 = State state
  | otherwise = buyDecision state
  where
    name = activePlayerId state
    treasures = filter isTreasure (hand (activePlayer state))

    playTreasureDecision = decision name (Choices QTreasures
                                                  treasures
                                                  (const True)
                                                  (\cards -> playAll cards name state))
                            True state
      `andThen` buyDecision

    buyDecision s2 = decision name (Choice QBuy
                                           affordableCards
                                           (\card -> plusBuys (-1) name s2
                                                     `andThen` plusMoney (- (cost card)) name
                                                     `andThen` buy card name
                                                     `andThen` buyPhase))
                                    True s2
      where
        moneyToSpend = money (turn s2)
        affordableCards = filter ((<=moneyToSpend) . cost) $ availableCards s2


actionPhase :: GameState -> GameStep
actionPhase state
  | actions (turn state) == 0 || actionsInHand == [] = State state
  | otherwise = decision (activePlayerId state) (Choice QPlay
                                                        actionsInHand
                                                        (\card -> plusActions (-1) (activePlayerId state) state
                                                                  `andThen` play card (activePlayerId state)
                                                                  `andThen` actionPhase))
                         True state
  where
    actionsInHand = filter isAction (hand (activePlayer state))

playTurn :: GameState -> GameStep
playTurn state =
  info (activePlayerId state) ("Your turn:\n" ++ show (visibleState (activePlayerId state) state)) state
  `andThen` actionPhase
  `andThen` buyPhase
  `andThen` cleanupPhase

finished :: GameState -> Bool
finished state =
  L.notElem "Province" (map cardName (availableCards state))
  || length (filter null (piles state)) >= 3
