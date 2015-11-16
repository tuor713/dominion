module Dominion.Model
  (CardType(..),
   Location(..),
   Edition(..),
   Card(..),
   Player(..),
   TurnState(..),
   GameState(..),
   RandomState,
   PlayStep,
   GameStack,
   ActionGenerator,
   Interaction(..),
   ActionStep(..),
   queueActions,
   queueSteps,
   points,
   allCards,
   isAction,
   isReaction,
   isTreasure,
   isStandardVictory,
   activePlayer,
   playerByName,
   updatePlayer,
   updatePlayerR,
   playerNames,
   opponentNames,
   opponents,
   availableCards,
   canDraw,
   execute,
   reshuffleDiscard,
   draw,
   newTurn,
   )
where


import qualified Control.Monad.Trans.State.Lazy as St
import qualified Data.List as L
import System.Random (StdGen, mkStdGen, randomR, newStdGen)


data CardType = Action | Treasure | Victory | CurseType {- This is a bit of a hack -} |
    Reaction | Attack | Duration |
    Prize | Looter | Ruins | Knight | Reserve | Traveller
    deriving (Eq, Read, Show)

data Location = Hand String | Discard String | TopOfDeck String | InPlay | Trash | Supply
  deriving (Eq, Show)

data Edition = Base | Intrigue | Seaside | Alchemy | Prosperity | Cornucopia | Hinterlands | DarkAges | Guilds
  deriving (Eq, Read, Show)


-- The fundamental domain types

data Card = Card { cardName :: String,
                   edition :: Edition,
                   cost :: Int, -- TODO this only works until Alchemy
                   types :: [CardType],
                   cardPoints :: Player -> Int,
                   onPlay :: GameState -> String -> GameState
                   }

instance Show Card where
  show = cardName

instance Eq Card where
  (==) c1 c2 = cardName c1 == cardName c2

instance Ord Card where
  c1 <= c2 = show c1 <= show c2

data Player = Player { name :: String,
                       hand :: [Card],
                       inPlay :: [Card],
                       deck :: [Card],
                       discardPile :: [Card]
                       }
                       deriving (Eq, Show)

data TurnState = TurnState { money :: Int,
                             buys :: Int,
                             actions :: Int,
                             stack :: GameStack
                             }
                             deriving (Show)

data GameState = GameState { players :: [Player],
                             trash :: [Card],
                             turn :: TurnState,
                             piles :: [[Card]],
                             gen :: StdGen
                             }

-- The game stack

type RandomState a = St.State StdGen a

type PlayStep = Either Interaction ActionStep

type GameStack = [PlayStep]

type ActionGenerator a = a -> GameStack

data Interaction =
  PickACard String String [Card] (ActionGenerator Card)
  | PickCards String String [Card] Int (ActionGenerator [Card])
  | YesNoDecision String String (ActionGenerator Bool)
  | OptionalDecision Interaction
  | Information String String

instance Show Interaction where
  show (PickACard player label cards _)     = "PickACard(" ++ player ++ "," ++ label ++ show cards ++ ")"
  show (PickCards player label cards num _) = "PickCards(" ++ player ++ "," ++ show num ++ "," ++ label ++ show cards ++ ")"
  show (YesNoDecision player label _)       = "YesNo(" ++ player ++ "," ++ label ++ ")"
  show (OptionalDecision child)             = "opt(" ++ show child ++ ")"
  show (Information player message)         = "info("++player++ "," ++ message ++ ")"

data ActionStep =
  GainGold Int
  | GainBuys Int
  | GainCard Location Card
  | GainActions Int
  | Draw String Int
  | PlayCard Card
  | PlayCopy Card
  | DiscardCard String Card
  | TrashCard String Card
  | TransferCard Card Location Location
  | Branch (GameState -> GameState)
  | Cleanup

instance Show ActionStep where
  show (GainGold num)         = "GainGold(" ++ show num ++ ")"
  show (GainBuys num)         = "GainBuys(" ++ show num ++ ")"
  show (GainCard loc c)       = "GainCard(" ++ show loc ++ "," ++ show c ++ ")"
  show (GainActions num)      = "GainActions(" ++ show num ++ ")"
  show (Draw player num)      = "Draw(" ++ player ++ "," ++ show num ++ ")"
  show (PlayCard c)           = "Play(" ++ show c ++ ")"
  show (PlayCopy c)           = "PlayCopy(" ++ show c ++ ")"
  show (DiscardCard player c) = "Discard(" ++ player ++ "," ++ show c ++ ")"
  show (TrashCard player c)   = "Trash(" ++ player ++ "," ++ show c ++ ")"
  show (TransferCard c from to) = "Transfer(" ++ show c ++ "," ++ show from ++ "," ++ show to ++ ")"
  show Cleanup                = "Cleanup"
  show (Branch _)             = "Branch"



-- Game mutator primitives

queueSteps :: GameState -> [PlayStep] -> GameState
queueSteps g steps = g { turn = (turn g) { stack = steps ++ stack (turn g) } }

queueActions :: GameState -> [ActionStep] -> GameState
queueActions g as = queueSteps g (map Right as)


-- Player

allCards :: Player -> [Card]
allCards s = concatMap (\f -> f s) [hand, inPlay, discardPile, deck]

points :: Player -> Int
points s = sum $ map (`cardPoints` s) $ allCards s


-- Extractors

activePlayer :: GameState -> Player
activePlayer state = head $ players state

playerByName :: GameState -> String -> Player
playerByName state pname = head $ filter ((== pname) . name) (players state)

updatePlayer :: GameState -> String -> (Player -> Player) -> GameState
updatePlayer state pname f =
  state { players = map (\p -> if name p == pname then f p else p) (players state) }

updatePlayerR :: GameState -> String -> (Player -> RandomState Player) -> GameState
updatePlayerR state pname f =
  state { players = ps, gen = resultgen }
  where
    (ps, resultgen) = St.runState (sequence $ map (\p -> if name p == pname then f p else return p) (players state)) (gen state)


playerNames :: GameState -> [String]
playerNames state = map name (players state)

opponentNames :: GameState -> String -> [String]
opponentNames state player = filter (/= player) (playerNames state)

opponents :: GameState -> String -> [Player]
opponents state player = filter ((/= player) . name) (players state)

availableCards :: GameState -> [Card]
availableCards g = concatMap (take 1) (piles g)

hasCardType :: Card -> CardType -> Bool
hasCardType card typ = typ `elem` types card

isAction card = hasCardType card Action
isReaction card = hasCardType card Reaction
isTreasure card = hasCardType card Treasure

-- TODO card as data has made this less type safe
isStandardVictory c
  | name == "Estate" = True
  | name == "Duchy" = True
  | name == "Province" = True
  | name == "Colony" = True
  | otherwise = False
  where
    name = cardName c


-- Actions

shuffle :: [a] -> RandomState [a]
shuffle ys = St.state $ \gen -> shuffle' gen ys []
  where
    shuffle' g [] acc = (acc,g)
    shuffle' g l acc = shuffle' gnew (lead ++ xs) (x:acc)
      where
        (k,gnew) = randomR (0, length l - 1) g
        (lead, x:xs) = splitAt k l


moveTo :: Card -> Location -> GameState -> GameState
moveTo c (Hand player) state = updatePlayer state player (\p -> p { hand = c : hand p })
moveTo c (Discard player) state = updatePlayer state player (\p -> p { discardPile = c : discardPile p })
moveTo c (TopOfDeck player) state = updatePlayer state player (\p -> p { deck = c : deck p })
moveTo c InPlay state = updatePlayer state (name (activePlayer state)) (\p -> p { inPlay = c : inPlay p })
moveTo c Trash state = state { trash = c : trash state }
moveTo c Supply state = error "Not implemented: moving card to supply"

moveFrom :: Card -> Location -> GameState -> GameState
moveFrom c (Hand player) state      = updatePlayer state player (\p -> p { hand = L.delete c $ hand p })
moveFrom c (Discard player) state   = updatePlayer state player (\p -> p { discardPile = L.delete c $ discardPile p })
moveFrom c (TopOfDeck player) state = updatePlayer state player (\p -> p { deck = L.delete c $ deck p })
moveFrom c InPlay state             = updatePlayer state (name (activePlayer state)) (\p -> p { inPlay = L.delete c $ inPlay p })
moveFrom c Trash state              = state { trash = L.delete c $ trash state }
moveFrom c Supply state             = state { piles = map takeFirst (piles state) }
  where
    takeFirst [] = []
    takeFirst (x:xs) = if x == c then xs else x:xs

transfer :: Card -> Location -> Location -> GameState -> GameState
transfer c from to state = moveTo c to (moveFrom c from state)

execute :: ActionStep -> GameState -> GameState
execute (GainGold num) state = state { turn = (turn state) { money = (money (turn state)) + num } }
execute (GainBuys num) state = state { turn = (turn state) { buys = (buys (turn state)) + num } }
execute (GainActions num) state = state { turn = (turn state) { actions = actions (turn state) + num } }
execute (GainCard toLoc card) state = transfer card Supply toLoc state
execute (DiscardCard player card) state = transfer card (Hand player) (Discard player) state
execute (TrashCard player card) state = transfer card (Hand player) Trash state
execute (Draw player num) state = updatePlayerR state player (`draw` num)
execute (PlayCopy card) state = onPlay card state (name (activePlayer state))
execute (TransferCard card from to) state = transfer card from to state
execute (PlayCard card) state = onPlay card (transfer card (Hand pname) InPlay state) pname
  where
    pname = name (activePlayer state)
execute (Branch f) state = f state
execute Cleanup state
  | finished state = state { turn = (turn state) { stack = [] } }
  | otherwise = state { players = tail (players state) ++ [current''],
                        turn = newTurn,
                        gen = gen' }
  where
    current = head (players state)
    current' = current { hand = [], inPlay = [], discardPile = hand current ++ inPlay current ++ discardPile current }
    (current'',gen') = St.runState (draw current' 5) (gen state)

-- Game Primitives

finished :: GameState -> Bool
finished state =
  L.notElem "Province" (map cardName (availableCards state))
  || length (filter null (piles state)) >= 3


canPlayAction :: GameState -> GameState
canPlayAction g =
  if actions (turn g) > 0 && length actionCards > 0
  then queueSteps g [Left (OptionalDecision (PickACard active "Choose action to play: " actionCards cont))]
  else g
  where
    active = name $ activePlayer g
    actionCards = filter isAction (hand (activePlayer g))
    cont card = map Right [GainActions (-1), PlayCard card, Branch canPlayAction]

selectBuy :: GameState -> GameState
selectBuy g = queueSteps g [Left (OptionalDecision (PickACard active "Choose a card to buy: " buyable cont))]
  where
    active = name $ activePlayer g
    buyable = filter ((<=availableMoney) . cost) (availableCards g)
    availableMoney = money (turn g)
    cont card = map Right [GainBuys (-1), GainGold (- (cost card)), GainCard (Discard active) card, Branch canBuyCard]

canBuyCard :: GameState -> GameState
canBuyCard g
  | noBuys > 0 && length treasures > 0 =
    queueSteps g [Left $ PickCards active "Choose treasures to play: " treasures (length treasures) tCont]
  | noBuys > 0 = selectBuy g
  | otherwise = g
  where
    active = name $ activePlayer g
    tCont cards = map Right $ map PlayCard cards ++ [Branch selectBuy]
    noBuys = buys (turn g)
    treasures = filter isTreasure (hand (activePlayer g))

newTurn :: TurnState
newTurn = TurnState { money = 0, buys = 1, actions = 1,
                      stack = map Right [Branch canPlayAction, Branch canBuyCard, Cleanup] }

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
    (!!!) p num = p { hand = (hand p) ++ take num (deck p), deck = drop num (deck p)}
