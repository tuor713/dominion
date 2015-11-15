{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Dominion (
  Edition(..),
  Card(..))
where

import Prelude hiding (interact)
import qualified Control.Monad.Trans.State.Lazy as St
import qualified Control.Monad as M
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import System.Random (StdGen, mkStdGen, randomR, newStdGen)
import Text.Read (readMaybe)

{-

== Modelling decisions ==
- how are triggers / reactions going to work
- what about global effects like Bridge
- model partial visibility
- do cards have an identity? (probably yes)
- should we model reveal ?

== Feature set ==
- non IO decision making like a bot

== Debt ==
- separate out random state completely
- make cards more data driven than all lengthy pattern matching
- separate domain model and card data / behaviour

-}


-- Foundational Types

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

-- Game locations
data Location = Hand String | Discard String | TopOfDeck String | InPlay | Trash | Supply
  deriving (Eq, Show)

type PlayStep = Either Interaction ActionStep

type GameStack = [PlayStep]

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

type RandomState a = St.State StdGen a

summarizeCards :: [Card] -> String
summarizeCards cards =
  cards |>
  L.sort |>
  L.group |>
  L.map (\cs -> (if (length cs > 1) then show (length cs) ++ "x " else "") ++ show (head cs)) |>
  L.intersperse ", " |>
  concat

instance Show GameState where
  show g = "Game {\n" ++
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

-- Card Knowledge Base

data Edition = Base | Intrigue | Seaside | Alchemy | Prosperity | Cornucopia |
  Hinterlands | DarkAges | Guilds
  deriving (Eq, Read, Show)

data CardType = Action | Treasure | Victory | CurseType {- This is a bit of a hack -} |
    Reaction | Attack | Duration |
    Prize | Looter | Ruins | Knight | Reserve | Traveller
    deriving (Eq, Read, Show)

data Card = Estate | Duchy | Province |
  Copper | Silver | Gold | Cellar | Chapel | Moat | Chancellor |
  Village | Woodcutter | Workshop | Bureaucrat | Feast |
  Gardens | Militia | Moneylender | Remodel | Smithy |
  Spy | Thief | ThroneRoom | CouncilRoom | Festival |
  Laboratory | Library | Market | Mine | Witch |
  Adventurer | Curse |

  -- Prosperity
  Platinum | Colony

  deriving (Eq, Read, Show)

instance Ord Card where
  c1 <= c2 = show c1 <= show c2


-- Utilities

-- Clojure / F# style threading
(|>) :: a -> (a -> b) -> b
(|>) x f = f x

shuffle :: [a] -> RandomState [a]
shuffle ys = St.state $ \gen -> shuffle' gen ys []
  where
    shuffle' g [] acc = (acc,g)
    shuffle' g l acc = shuffle' gnew (lead ++ xs) (x:acc)
      where
        (k,gnew) = randomR (0, length l - 1) g
        (lead, x:xs) = splitAt k l

list2MultiSet :: Ord a => [a] -> Map.Map a Int
list2MultiSet xs = Map.fromListWith (+) $ map (,1) xs

contains :: Ord a => Map.Map a Int -> Map.Map a Int -> Bool
contains bigger smaller = Map.isSubmapOfBy (<=) smaller bigger


-- Basic operations

mainloop :: GameState -> IO ()
mainloop state =
  do
    next <- step state
    -- no actions means end of game
    if null (stack (turn next))
    then do
          putStrLn "Game Finished!"
          putStrLn $ "Final score: " ++ (players next
                                         |> L.sortOn points
                                         |> reverse
                                         |> map (\p -> name p ++ ": " ++ show (points p))
                                         |> L.intersperse ", "
                                         |> concat)
    else mainloop next

playOnConsole :: [String] -> [Card] -> IO ()
playOnConsole players cards =
  do
    gen <- newStdGen
    mainloop $ mkGame players cards gen

step :: GameState -> IO GameState
step g =
  case stack (turn g) of
    (Left decision:steps) -> do putStrLn (show g)
                                newSteps <- interact decision
                                return $ g { turn = (turn g) { stack = (newSteps ++ steps) } }
    (Right action:steps) -> return $ execute action g { turn = (turn g) { stack = steps } }
    _ -> error "Illegal state, no steps but game has not ended"

getInput :: String -> (String -> Maybe a) -> IO a
getInput caption parser =
  do
    putStrLn caption
    putStr ">> "
    line <- getLine
    case parser line of
      Just v -> return v
      Nothing -> getInput caption parser


decision2prompt :: Interaction -> (String, String -> Maybe GameStack)
decision2prompt (PickACard player caption choices cont) =
  ("[" ++ player ++ "] " ++ caption ++ summarizeCards choices,
   (fmap cont) . (\s -> readMaybe s >>= \x -> if x `elem` choices then Just x else Nothing))

decision2prompt (PickCards player caption choices max cont) =
  ("[" ++ player ++ "] " ++ caption ++ summarizeCards choices,
   (fmap cont) . (\s -> if s == "all" then Just choices else readMaybe s
                        >>= \xs -> if length xs <= max then Just xs else Nothing
                        >>= \xs -> if contains (list2MultiSet choices) (list2MultiSet xs) then Just xs else Nothing))

decision2prompt (OptionalDecision decision) =
  (caption, handler')
  where
    (caption, handler) = decision2prompt decision
    handler' s = if s == "" then Just [] else handler s

decision2prompt (YesNoDecision player caption cont) =
  ("[" ++ player ++ "] " ++ caption ++ " [yn]", parse)
  where
    parse "y" = Just (cont True)
    parse "n" = Just (cont False)
    parse _ = Nothing

-- TODO this is partial because we don't support Information
decision2prompt (Information _ _) = undefined

interact (Information player message) =
  do putStrLn ("[" ++ player ++ "] " ++ message)
     return []

interact decision = getInput caption handler
  where
    (caption, handler) = decision2prompt decision

-- Hand String | Discard String | TopOfDeck String | InPlay | Trash | Supply

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
execute (PlayCopy card) state = play card (name (activePlayer state)) state
execute (TransferCard card from to) state = transfer card from to state
execute (PlayCard card) state = play card pname $ transfer card (Hand pname) InPlay state
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

initialDeck :: [Card]
initialDeck = replicate 7 Copper ++ replicate 3 Estate

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

mkPlayer :: String -> RandomState Player
mkPlayer name = draw Player { name = name, hand = [], discardPile = initialDeck, deck = [], inPlay = [] } 5

mkGame :: [String] -> [Card] -> StdGen -> GameState
mkGame names kingdomCards gen =
  GameState { players = players,
              piles = map (\c -> replicate (noInPile c) c) (standardCards ++ kingdomCards),
              trash = [],
              turn = newTurn,
              gen = gen'
              }
  where
    standardCards = [Estate,Duchy,Province,Copper,Silver,Gold,Curse]
    playerNo = length names
    noInPile card
      | isStandardVictory card && playerNo == 2 = 8
      | isStandardVictory card = 12
      | card == Curse && playerNo == 2 = 10
      | card == Curse && playerNo == 3 = 20
      | card == Curse = 30
      | card == Gold = 30
      | card == Silver = 40
      | card == Copper = 60 - 7 * playerNo
      | otherwise = 10

    (players,gen') = St.runState (sequence $ map mkPlayer names) gen

finished :: GameState -> Bool
finished state =
  L.notElem Province (availableCards state)
  || length (filter null (piles state)) >= 3

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


allCards :: Player -> [Card]
allCards s = concatMap (\f -> f s) [hand, inPlay, discardPile, deck]

points :: Player -> Int
points s = sum $ map (cardPoints s) $ allCards s


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
hasCardType card typ = typ `elem` cardTypes card

isAction card = hasCardType card Action
isReaction card = hasCardType card Reaction
isTreasure card = hasCardType card Treasure
isStandardVictory Estate = True
isStandardVictory Duchy = True
isStandardVictory Province = True
isStandardVictory Colony = True
isStandardVictory _ = False

-- Knowledge base

cost :: Card -> Int
-- Base
cost Estate = 2
cost Duchy = 5
cost Province = 8
cost Copper = 0
cost Silver = 3
cost Gold = 6
cost Cellar = 2
cost Chapel = 2
cost Moat = 2
cost Chancellor = 3
cost Village = 3
cost Woodcutter = 3
cost Workshop = 3
cost Bureaucrat = 4
cost Feast = 4
cost Gardens = 4
cost Militia = 4
cost Moneylender = 4
cost Remodel = 4
cost Smithy = 4
cost Spy = 4
cost Thief = 4
cost ThroneRoom = 4
cost CouncilRoom = 5
cost Festival = 5
cost Laboratory = 5
cost Library = 5
cost Market = 5
cost Mine = 5
cost Witch = 5
cost Adventurer = 6
cost Curse = 0

-- Prosperity
cost Platinum = 9
cost Colony = 11


cardPoints :: Player -> Card -> Int
cardPoints _ Estate = 1
cardPoints _ Duchy = 3
cardPoints _ Province = 6
cardPoints p Gardens = length (allCards p) `quot` 10
cardPoints _ Curse = -1
cardPoints _ Colony = 10
cardPoints _ _ = 0

cardTypes :: Card -> [CardType]
cardTypes Copper = [Treasure]
cardTypes Silver = [Treasure]
cardTypes Gold = [Treasure]
cardTypes Curse = [CurseType]
cardTypes Estate = [Victory]
cardTypes Duchy = [Victory]
cardTypes Province = [Victory]

cardTypes Cellar = [Action]
cardTypes Chapel = [Action]
cardTypes Moat = [Action, Reaction]
cardTypes Chancellor = [Action]
cardTypes Village = [Action]
cardTypes Woodcutter = [Action]
cardTypes Workshop = [Action]
cardTypes Bureaucrat = [Action, Attack]
cardTypes Feast = [Action]
cardTypes Gardens = [Victory]
cardTypes Militia = [Action, Attack]
cardTypes Moneylender = [Action]
cardTypes Remodel = [Action]
cardTypes Smithy = [Action]
cardTypes Spy = [Action, Attack]
cardTypes Thief = [Action, Attack]
cardTypes ThroneRoom = [Action]
cardTypes CouncilRoom = [Action]
cardTypes Festival = [Action]
cardTypes Laboratory = [Action]
cardTypes Library = [Action]
cardTypes Market = [Action]
cardTypes Mine = [Action]
cardTypes Witch = [Action, Attack]
cardTypes Adventurer = [Action]

cardTypes Platinum = [Treasure]
cardTypes Colony = [Victory]


queueSteps :: GameState -> [PlayStep] -> GameState
queueSteps g steps = g { turn = (turn g) { stack = steps ++ stack (turn g) } }

queueActions :: GameState -> [ActionStep] -> GameState
queueActions g as = queueSteps g (map Right as)


play :: Card -> String -> GameState -> GameState
play Copper _ g = queueActions g [GainGold 1]
play Silver _ g = queueActions g [GainGold 2]
play Gold _ g = queueActions g [GainGold 3]
play Platinum _ g = queueActions g [GainGold 5]

play Cellar p g =
  queueSteps g [Right (GainActions 1),
                Left (PickCards p
                                ("Choose any number of cards to discard: ")
                                (hand (playerByName g p))
                                (length (hand (playerByName g p)))
                                cont)]
  where
    cont cards = map Right $ map (DiscardCard p) cards ++ [Draw p (length cards)]

play Chapel p g = queueSteps g [Left (PickCards p "Choose up to 4 cards to trash: " (hand (playerByName g p)) 4 cont)]
  where
    cont cards = map (Right . TrashCard p) cards

-- TODO implement the reaction part
play Moat p g = queueActions g [Draw p 2]

play Chancellor p g = queueSteps g [Right $ GainGold 2,
                                    Left $ (YesNoDecision p "Put deck into discard pile?"
                                                          (\b -> if b then [Right $ Branch deckToDiscard] else []))]
  where
    -- Note that this is correct in as far as cards are not discarded individually
    deckToDiscard g = updatePlayer g p (\player -> player { deck = [], discardPile = deck player ++ discardPile player })

play Village p g = queueActions g [Draw p 1, GainActions 2]
play Woodcutter _ g = queueActions g [GainBuys 1, GainGold 2]
play Workshop p g = queueSteps g [Left (PickACard p "Choose a card to gain: " (filter ((<=4) . cost) (availableCards g)) cont)]
  where
    cont card = [Right (GainCard (Discard p) card)]

play Bureaucrat p g = queueSteps g $ [Right $ GainCard (TopOfDeck p) Silver] ++ opDecisions
  where
    opDecisions = opponents g p
                  |> filter (not . null . filter isTreasure . hand)
                  |> map (\p -> Left $ PickACard (name p) "Choose a treasure to put on your deck: " (filter isTreasure (hand p))
                                       (\c -> [Right $ TransferCard c (Hand (name p)) (TopOfDeck (name p))]))

play Feast p g = queueSteps g' [Left (PickACard p "Choose a card to gain: " (filter ((<=5) . cost) (availableCards g')) cont)]
  where
    cont card = [Right (GainCard (TopOfDeck p) card)]
    g' = updatePlayer g p (\player -> player { inPlay = L.delete Feast (inPlay player) })

play Militia p g = queueSteps g ([Right (GainGold 2)]
                                 ++ Maybe.catMaybes (map (decision g) (opponentNames g p)))
  where
    decision g op = if length h <= 3
                    then Nothing
                    else Just $ Left $ PickCards p "Choose cards to discard:" h (length h - 3) (cont op)
      where
        h = hand (playerByName g op)
    cont op cards = map (Right . DiscardCard op) cards

-- TODO the linkage is imperfect because reactions and replacement effects could still happen
play Moneylender p g
  | Copper `elem` h = queueActions g [TrashCard p Copper, GainGold 3]
  | otherwise = g
  where
    h = hand (playerByName g p)

play Remodel p g
  | null h = g -- Nothing happens since there is nothing to trash
  | otherwise = queueSteps g [Left (PickACard p "Choose a card to trash: " h cont)]
  where
    h = hand (playerByName g p)
    cont card = [Left (PickACard p "Choose a card to gain: "
                                 (filter ((<=maxCost) . cost) (availableCards g))
                                 (\c -> [Right $ GainCard (Discard p) c]))]
      where
        maxCost = 2 + cost card


play Smithy p g = queueActions g [Draw p 3]

play Spy p g = queueActions g [Draw p 1, GainActions 1, Branch lookAtTopCards]
  where
    lookAtTopCards game = queueSteps game { gen = gen', players = ps } $ map (Left . discardDecision) $ filter canDraw ps
      where
        (ps, gen') = St.runState (sequence $ map reshuffleIfNeeded (players game)) (gen game)
        reshuffleIfNeeded p = if null (deck p) then reshuffleDiscard p else return p
        discardDecision player = YesNoDecision p ("Discard top of deck (" ++ show top ++ ") for player " ++ name player ++ "?")
                                              (\c -> [Right $ TransferCard top (TopOfDeck (name player)) (Discard (name player))])
          where
            top = head (deck player)

-- TODO implementation is messy and also imprecise because we ask trash/gain per card not for all at the end
play Thief thief g = queueSteps g' decisions
  where
    g' = g { players = ps, gen = gen' }
    (ps, gen') = St.runState (sequence $ map reshuffleIfNeeded (players g)) (gen g)
    reshuffleIfNeeded p = if name p /= thief && length (deck p) < 2 then reshuffleDiscard p else return p
    decisions = opponents g' thief |> filter (null . deck) |> (\ops -> map reveal ops ++ concatMap mkDecision ops)
    reveal opponent = Left $ Information "All" (name opponent ++ " revealed " ++ show top2)
      where
        top2 = take 2 (deck opponent)
    mkDecision opponent
      | any isTreasure top2 =
        [Left $ PickACard thief "Pick a treasure to trash: " (filter isTreasure top2) cont]
      | otherwise = []
      where
        top2 = take 2 (deck opponent)
        cont c = [Left $ YesNoDecision thief ("Gain " ++ show c ++ " from trash?") (cont2 c)]
        cont2 c True = [Right $ TransferCard c (TopOfDeck (name opponent)) Trash,
                        Right $ TransferCard c Trash (Discard thief)]
                       ++ discardRest c
        cont2 c False = [Right $ TransferCard c (TopOfDeck (name opponent)) Trash] ++ discardRest c
        discardRest c = map (\c -> Right $ TransferCard c (TopOfDeck (name opponent)) (Discard (name opponent))) $ L.delete c top2


play ThroneRoom p g = queueSteps g [Left (PickACard p "Choose an action: " (filter isAction (hand (playerByName g p))) cont)]
  where
    cont card = map Right [PlayCard card, PlayCopy card]

play CouncilRoom player g = queueActions g ([Draw player 4, GainBuys 1] ++ map (`Draw` 1) (opponentNames g player))
play Festival _ g = queueActions g [GainActions 2, GainBuys 1, GainGold 2]
play Laboratory p g = queueActions g [Draw p 2, GainActions 1]

-- TODO potentially the "aside" part needs to be modelled explicitly
play Library p g = queueActions g [Branch repeatDraw]
  where
    condDraw s = if isAction newCard
                 then queueSteps s' [Left (YesNoDecision p ("Set aside action " ++ show newCard ++ "?") cont)]
                 else s'
      where
        s' = execute (Draw p 1) s
        newCard = head $ (hand (playerByName s' p) L.\\ hand (playerByName s p))
        cont True = [Right $ DiscardCard p newCard]
        cont False = []
    repeatDraw s
      | length (hand (activePlayer s)) < 7 && canDraw (activePlayer s) =
        queueActions s [Branch condDraw, Branch repeatDraw]
      | otherwise = s


play Market p g = queueActions g [Draw p 1, GainActions 1, GainBuys 1, GainGold 1]

play Mine p g = if length treasuresInHand == 0
                then g
                else queueSteps g [Left (PickACard p "Choose a treasure to trash: "
                                                   treasuresInHand
                                                   cont)]
  where
    treasuresInHand = (filter isTreasure (hand (playerByName g p)))
    cont card = [Right (TrashCard p card),
                 Left (PickACard p "Choose a treasure card: "
                                 (filter (\c -> cost c <= cost card + 3 && isTreasure c) (availableCards g))
                                 cont2)]
    cont2 card = [Right (GainCard (Hand p) card)]

play Witch player g =
  queueActions g ([Draw player 2] ++ map (\p -> GainCard (Discard p) Curse) (opponentNames g player))

play Adventurer player g = updatePlayerR g player inner
  where
    inner :: Player -> RandomState Player
    inner p = do (p', cards) <- drawIter p 2 []
                 return p' { discardPile = filter (not . isTreasure) cards ++ discardPile p',
                             hand = filter isTreasure cards ++ hand p' }

    drawIter :: Player -> Int -> [Card] -> RandomState (Player, [Card])
    drawIter p 0 cards = return (p, cards)
    drawIter p num cards =
      case p of
        Player { deck = [], discardPile = [] } -> return (p, cards)
        Player { deck = (x:xs) } -> drawIter (p {deck = xs}) (if isTreasure x then num-1 else num) (x:cards)
        _ -> reshuffleDiscard p >>= \p2 -> drawIter p2 num cards

play c _ _ = error ("Card not implemented: " ++ (show c))