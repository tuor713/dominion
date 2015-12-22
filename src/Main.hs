{-# LANGUAGE OverloadedStrings, FlexibleInstances, TupleSections, ScopedTypeVariables #-}
module Main where

import Dominion
import Dominion.Model
import Dominion.Cards
import Dominion.Bots hiding (buys)
import Dominion.Stats
import Dominion.Web.Pages

import Control.Applicative
import Control.Concurrent
import Control.Monad.IO.Class
import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server
import qualified Data.Aeson as J
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.IORef as IORef
import Data.List.Split (wordsBy)
import qualified Data.Maybe as Maybe
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified System.Environment as Env
import System.Random (newStdGen)

import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Text.Blaze.Html5 as H



main :: IO ()
main = Env.getArgs >>= \(cmd:args) ->
  case cmd of
    "run" -> runConsole args
    "web" -> runWeb
    _ -> return ()


runConsole :: [String] -> IO ()
runConsole [] = runConsole ["1000"]
runConsole (num:_) =
  do
    gen <- newStdGen
    let stats = evalSim (runSimulations
                          [("Alice",bigSmithy "Alice"), ("Bob",doubleJack "Bob")]
                          tableau
                          (emptyStats ["Alice","Bob"])
                          (read num))
                        gen
    putStrLn (showStats stats)
  where
    tableau = map lookupCard ["market", "library", "smithy", "cellar", "chapel", "militia",
                              "village", "laboratory", "witch", "jack of all trades"]

runWeb :: IO ()
runWeb = do
  gamesRepository <- IORef.newIORef (0,Map.empty)
  quickHttpServe (site gamesRepository)

homePage :: Snap ()
homePage = writeHtml htmlHomePage

site :: IORef.IORef GameRepository -> Snap ()
site gamesRepository =
    ifTop homePage <|>
    route [ ("simulation", method GET $ simulationHandler),
            ("game/start", method POST $ startGameHandler gamesRepository),
            ("game/play", method GET $ setupGameHandler),
            ("game/suggested", method GET $ suggestedTableauHandler),
            ("game/:id/decision/:player", method GET $ decisionHandler gamesRepository),
            ("game/:id/decision/:player", method POST $ makeDecisionHandler gamesRepository)
          ] <|>
    dir "static" (serveDirectory "static")


writeHtml :: H.Html -> Snap ()
writeHtml = writeLBS . renderHtml


-- Game API

data PlayerState = Bot Bot | External (MVar GameStep)
type Game = (Map.Map PlayerId PlayerState, SimulationState)

type GameRepository = (Int,Map.Map Int Game)

externalDecision :: PlayerState -> MVar GameStep
externalDecision (External mvar) = mvar
externalDecision (Bot _) = error "Called externalDecision on internal bot"

playerUpdateState :: PlayerState -> GameState -> IO ()
playerUpdateState (Bot _) _ = return ()
playerUpdateState (External mvar) state = putMVar mvar (State state)

playerDecision :: Game -> PlayerState -> PlayerId -> GameState -> Decision -> IO (Maybe (Game,GameStep))
playerDecision (states,simState) (Bot bot) _ state decision = do
  sim <- bot state decision
  let (next,simState') = runSim sim (sim2Gen simState)
  return $ Just ((states,combineSimStates simState simState'),next)

playerDecision _ (External mvar) id state decision =
  putMVar mvar (Decision id state decision) >> return Nothing

playerStates :: Game -> Map.Map PlayerId PlayerState
playerStates = fst

nextStep :: Game -> GameStep -> IO Game
nextStep game@(states,simState) (State state)
  | finished state = sequence (map (\p -> playerUpdateState p state) $ Map.elems $ playerStates game) >> return game
  | otherwise = do
    let (next,simState') = runSim (playTurn (activePlayerId state) state) (sim2Gen simState)
    nextStep (states,combineSimStates simState simState') next

nextStep game (Decision p state dec) = do
  next <- playerDecision game ((playerStates game) Map.! p) p state dec
  case next of
    Just (game',step) -> nextStep game' step
    Nothing -> return game


setupGameHandler :: Snap ()
setupGameHandler = writeHtml htmlSetupGame

suggestedTableauHandler :: Snap ()
suggestedTableauHandler = (writeHtml . htmlSuggestedTableaus)
  [("First Game",               ["cellar", "market", "militia", "mine", "moat", "remodel", "smithy", "village", "woodcutter", "workshop"]),
   ("Big Money",                ["adventurer", "bureaucrat", "chancellor", "chapel", "feast", "laboratory", "market", "mine", "moneylender", "throne room"]),
   ("Interaction",              ["bureaucrat", "chancellor", "council room", "festival", "library", "militia", "moat", "spy", "thief", "village"]),
   ("Size Distortion",          ["cellar","chapel","feast","gardens","laboratory","thief","village","witch","woodcutter","workshop"]),
   ("Village Square",           ["bureaucrat","cellar","festival","library","market","remodel","smithy","throne room","village","woodcutter"]),

   ("Deconstruction",           ["bridge", "mining village", "remodel", "saboteur", "secret chamber", "spy", "swindler", "thief", "throne room", "torturer"]),
   ("Hand Madness",             ["bureaucrat", "chancellor", "council room", "courtyard", "mine", "militia", "minion", "nobles", "steward", "torturer"]),
   ("Underlings",               ["baron", "cellar", "festival", "library", "masquerade", "minion", "nobles", "pawn", "steward", "witch"]),

   ("Reach for Tomorrow",       ["adventurer", "cellar", "council room", "cutpurse", "ghost ship", "lookout", "sea hag", "spy", "treasure map", "village"]),
   ("Repetition",               ["caravan", "chancellor", "explorer", "festival", "militia", "outpost", "pearl diver", "pirate ship", "treasury", "workshop"]),
   ("Give and Take",            ["ambassador", "fishing village", "haven", "island", "library", "market", "moneylender", "salvager", "smugglers", "witch"]),

   ("Forbidden Arts",           ["apprentice", "familiar", "possession", "university", "cellar", "council room", "gardens", "laboratory", "thief", "throne room"]),
   ("Potion Mixers",            ["alchemist", "apothecary", "golem", "herbalist", "transmute", "cellar", "chancellor", "festival", "militia", "smithy"]),
   ("Chemistry Lesson",         ["alchemist", "golem", "philosopher's stone", "university", "bureaucrat", "market", "moat", "remodel", "witch", "woodcutter"]),

   ("Biggest Money",            ["bank", "grand market", "mint", "royal seal", "venture", "adventurer", "laboratory", "mine", "moneylender", "spy"]),
   ("The King's Army",          ["expand", "goons", "king's court", "rabble", "vault", "bureaucrat", "cellar", "chancellor", "gardens", "village"]),
   ("The Good Life",            ["contraband", "counting house", "hoard", "monument", "mountebank", "bureaucrat", "cellar", "chancellor", "gardens", "village"]),

   ("Bounty of the Hunt",       ["harvest", "horn of plenty", "hunting party", "menagerie", "tournament", "cellar", "festival", "militia", "moneylender", "smithy"]),
   ("Bad Omens",                ["fortune teller", "hamlet", "horn of plenty", "jester", "remake", "adventurer", "bureaucrat", "laboratory", "spy", "throne room"]),
   ("The Jester's Workshop",    ["fairgrounds", "farming village", "horse traders", "jester", "young witch", "feast", "laboratory", "market", "remodel", "workshop"]),

   ("Highway Robbery",          ["cellar", "library", "moneylender", "throne room", "workshop", "highway", "inn", "margrave", "noble brigand", "oasis"]),
   ("Adventures Abroad",        ["adventurer", "chancellor", "festival", "laboratory", "remodel", "crossroads",  "farmland", "fool's gold", "oracle", "spice merchant"]),

   ("High and Low",             ["hermit", "hunting grounds", "mystic", "poor house", "wandering minstrel", "cellar", "moneylender", "throne room", "witch", "workshop"]),
   ("Chivalry and Revelry",     ["altar", "knights", "rats", "scavenger", "squire", "festival", "gardens", "laboratory", "library", "remodel"]),

   ("Victory Dance",            ["bridge", "duke", "great hall", "harem", "ironworks", "masquerade", "nobles", "pawn", "scout", "upgrade"]),
   ("Secret Schemes",           ["conspirator", "harem", "ironworks", "pawn", "saboteur", "shanty town", "steward", "swindler", "trading post", "tribute"]),
   ("Best Wishes",              ["coppersmith", "courtyard", "masquerade", "scout", "shanty town", "steward", "torturer", "trading post", "upgrade", "wishing well"]),

   ("High Seas",                ["bazaar", "caravan", "embargo", "explorer", "haven", "island", "lookout", "pirate ship", "smugglers", "wharf"]),
   ("Buried Treasure",          ["ambassador", "cutpurse", "fishing village", "lighthouse", "outpost", "pearl diver", "tactician", "treasure map", "warehouse", "wharf"]),
   ("Shipwreckers",             ["ghost ship", "merchant ship", "native village", "navigator", "pearl diver", "salvager", "sea hag", "smugglers", "treasury", "warehouse"]),

   ("Beginners",                ["bank", "counting house", "expand", "goons", "monument", "rabble", "royal seal", "venture", "watchtower", "worker's village"]),
   ("Friendly Interactive",     ["bishop", "city", "contraband", "forge", "hoard", "peddler", "royal seal", "trade route", "vault", "worker's village"]),
   ("Big Actions",              ["city", "expand", "grand market", "king's court", "loan", "mint", "quarry", "rabble", "talisman", "vault"]),

   ("Paths to Victory",         ["bishop", "counting house", "goons", "monument", "peddler", "baron", "harem", "pawn", "shanty town", "upgrade"]),
   ("All Along the Watchtower", ["hoard", "talisman", "trade route", "vault", "watchtower", "bridge", "great hall", "mining village", "pawn", "torturer"]),
   ("Lucky Seven",              ["bank", "expand", "forge", "king's court", "vault", "bridge", "coppersmith", "swindler", "tribute", "wishing well"])
   ]

data StartGameReq = StartGameReq { gamePlayers :: [String], gameCards :: [String], gameType :: GameType }
  deriving (Eq,Show)

instance J.FromJSON GameType where
  parseJSON (J.String s)
    | s == "colony" = return ColonyGame
    | otherwise = return StandardGame
  parseJSON _ = return StandardGame

instance J.FromJSON StartGameReq where
  parseJSON (J.Object v) = StartGameReq <$>
    v J..: "players" <*>
    v J..: "cards" <*>
    v J..: "type"
  parseJSON _ = empty

startGameHandler :: IORef.IORef GameRepository -> Snap ()
startGameHandler gamesRepository = do
  body <- readRequestBody 10000
  let req = J.decode body :: Maybe StartGameReq
  let (players,cards,gameTyp) = maybe (["Alice","Bob"],defaultTableau,StandardGame) (\req -> (gamePlayers req, gameCards req, gameType req)) req
  let tableau = map lookupCard cards

  id <- liftIO $ do
    (nextId,games) <- IORef.readIORef gamesRepository
    gen <- newStdGen
    let (newGame,simState) = runSim (mkGame gameTyp players tableau) gen

    aliceVar <- newEmptyMVar
    let mvars = Map.fromList
                  ((head players, External aliceVar) :
                    (map
                      (\name -> (name, Bot $ aiBot (doubleJack name)))
                      (tail players)))

    game <- nextStep (mvars,simState) (State newGame)
    IORef.writeIORef gamesRepository (nextId+1, Map.insert nextId game games)
    return nextId
  writeBS $ C8.pack $ show id
  where
    defaultTableau = ["market", "library", "smithy", "cellar", "chapel", "militia",
                      "village", "laboratory", "witch", "jack of all trades"]

jMap :: J.ToJSON a => Map.Map String a -> J.Value
jMap m = J.object $ map (\(k,v) -> T.pack k J..= J.toJSON v) $ Map.toList m

jString :: String -> J.Value
jString = J.String . T.pack

instance J.ToJSON Card where
  toJSON card = J.toJSON (typ card)

instance J.ToJSON CardDef where
  toJSON card = J.String $ T.pack (cardName card)

instance J.ToJSON Trigger where
  toJSON AttackTrigger = J.String "attack"
  toJSON BuyTrigger = J.String "buy"
  toJSON GainTrigger = J.String "gain"
  toJSON TrashTrigger = J.String "trash"
  toJSON StartOfTurnTrigger = J.String "startOfTurn"
  toJSON StartOfGameTrigger = J.String "startOfGame"

instance J.ToJSON Location where
  toJSON Supply         = J.toJSON [J.String "supply"]
  toJSON (Hand p)       = J.toJSON [J.String "hand" , jString p]
  toJSON (Discard p)    = J.toJSON [J.String "discardPile", jString p]
  toJSON Trash          = J.toJSON [J.String "trash"]
  toJSON (TopOfDeck p)  = J.toJSON [J.String "topOfDeck", jString p]
  toJSON InPlay         = J.toJSON [J.String "inPlay"]
  toJSON InPlayDuration = J.toJSON [J.String "inPlayDuration"]
  toJSON (Mat p mat)    = J.toJSON [J.String "mat", jString p, jString (show mat)]

instance J.ToJSON Effect where
  toJSON (EffectPlusCards no)          = J.toJSON [J.String "plusCards", J.toJSON no]
  toJSON (EffectPlusActions no)        = J.toJSON [J.String "plusActions", J.toJSON no]
  toJSON (EffectUseTokens token)       = J.toJSON [J.String "useTokens", jString (show token)]
  toJSON (EffectPlusBuys no)           = J.toJSON [J.String "plusBuys", J.toJSON no]
  toJSON (EffectPlusMoney no)          = J.toJSON [J.String "plusMoney", J.toJSON no]
  toJSON (EffectDiscardNo no)          = J.toJSON [J.String "discardNo", J.toJSON no]
  toJSON (EffectTrashNo no)            = J.toJSON [J.String "trashNo", J.toJSON no]
  toJSON (EffectDiscard card from)     = J.toJSON [J.String "discard", J.toJSON card, J.toJSON from]
  toJSON (EffectBuy card)              = J.toJSON [J.String "buy", J.toJSON card]
  toJSON (EffectGain card to)          = J.toJSON [J.String "gain", J.toJSON card, J.toJSON to]
  toJSON (EffectGainFrom card from to) = J.toJSON [J.String "gainFrom", J.toJSON card, J.toJSON from, J.toJSON to]
  toJSON (EffectPass card from to)     = J.toJSON [J.String "pass", J.toJSON card, J.toJSON from, J.toJSON to]
  toJSON (EffectPut card from to)      = J.toJSON [J.String "put", J.toJSON card, J.toJSON from, J.toJSON to]
  toJSON (EffectTrash card from)       = J.toJSON [J.String "trash", J.toJSON card, J.toJSON from]
  toJSON (EffectReveal card)           = J.toJSON [J.String "reveal", J.toJSON card]
  toJSON (EffectPlayAction card)       = J.toJSON [J.String "playAction", J.toJSON card]
  toJSON (EffectPlayCopy card)         = J.toJSON [J.String "playCopy", J.toJSON card]
  toJSON (EffectPlayTreasure card)     = J.toJSON [J.String "playTreasure", J.toJSON card]
  toJSON (SpecialEffect card)          = J.toJSON [J.String "useAbility", J.toJSON card]
  toJSON NullEffect                    = J.toJSON [J.String "nullEffect"]

instance J.ToJSON Decision where
  toJSON (ChooseToUse effect _) = J.object ["type"   J..= J.String "use",
                                            "effect" J..= T.pack (show effect)]

  toJSON (ChooseNumber effect (lo,hi) _ ) = J.object ["type" J..= J.String "chooseNumber",
                                                      "effect" J..= T.pack (show effect),
                                                      "min"    J..= J.toJSON lo,
                                                      "max"    J..= J.toJSON hi
                                                      ]

  toJSON (ChooseCard effect choices _) = J.object ["type"   J..= J.String "chooseCard",
                                                   "cards"  J..= J.Array (V.fromList (map J.toJSON choices)),
                                                   "effect" J..= T.pack (show effect)]

  toJSON (ChooseCards effect choices (lo,hi) _) = J.object ["type"   J..= J.String "chooseCards",
                                                            "cards"  J..= J.Array (V.fromList (map J.toJSON choices)),
                                                            "min"    J..= J.toJSON lo,
                                                            "max"    J..= J.toJSON hi,
                                                            "action" J..= J.toJSON (show effect)
                                                            ]

  toJSON (ChooseToReact card trigger _) = J.object ["type" J..= J.String "react",
                                                    "card" J..= J.toJSON card,
                                                    "trigger" J..= J.toJSON trigger]

  toJSON (ChooseEffects no effects _) = J.object ["type" J..= J.String "chooseEffects",
                                                  "num" J..= J.toJSON no,
                                                  "effects" J..= J.Array (V.fromList (map J.toJSON effects))
                                                  ]

  toJSON (Optional inner _) = J.object ["type"     J..= J.String "optional",
                                        "decision" J..= J.toJSON inner]

instance J.ToJSON Player where
  toJSON player = J.object $ zip ["hand","deck","inPlay","discard"] $ map (jMap . cardMap . ($ player)) [hand, deck, inPlay, discardPile]
    where
      cardMap :: [Card] -> Map.Map String Int
      cardMap cards = Map.mapKeys (cardName . typ) $ Map.fromListWith (+) (map (,1) cards)

instance J.ToJSON GameState where
  toJSON state = J.object ["turnOrder" J..= J.Array (V.fromList $ map (J.String . T.pack) $ turnOrder state),
                           "players" J..= jMap (players state),
                           "piles" J..= (jMap $ Map.mapKeys cardName $ piles state),
                           "ply" J..= J.toJSON (ply state)]

showInfo (vis,info) = "@" ++ show vis ++ " " ++ info

instance J.ToJSON (GameState,[Info],Decision) where
  toJSON (state,infos,decision) = J.object ["state"    J..= J.toJSON state,
                                            "gameLog"  J..= J.Array (V.fromList $ map (J.toJSON . showInfo) infos),
                                            "decision" J..= J.toJSON decision
                                            ]

nextDecision :: IORef.IORef GameRepository -> Int -> PlayerId -> Snap ()
nextDecision gamesRepository gameId player = do
  format <- getParam "format"
  let useHtml = maybe False (=="html") format
  let formatHandler = if useHtml then (renderHtml . (htmlDecision player)) else J.encode

  (_,games) <- liftIO $ IORef.readIORef gamesRepository

  let (states,simState) = games Map.! gameId
  let mvar = externalDecision $ states Map.! player


  decision <- liftIO $ readMVar mvar
  case decision of
    (State state) -> if useHtml
                     then writeHtml (htmlFinished state (sim2Infos simState))
                     else writeBS "{'status':'finished'}"
    (Decision _ state dec) -> writeLBS $ formatHandler (visibleState player state, filter (canSee player) $ sim2Infos simState, dec)



decisionHandler :: IORef.IORef GameRepository -> Snap ()
decisionHandler gamesRepository = do
  id <- getParam "id"
  player <- getParam "player"

  let gameId = fst $ Maybe.fromJust $ C8.readInt (Maybe.fromJust id)
  let playerId = C8.unpack (Maybe.fromJust player)

  nextDecision gamesRepository gameId playerId

parseDecision :: Decision -> String -> Simulation
parseDecision (ChooseToUse _ f) input           = f (input == "true")
parseDecision (ChooseNumber _ _ f) input        = f $ fst $ Maybe.fromJust $ C8.readInt $ C8.pack input
parseDecision (ChooseToReact _ _ f) input       = f (input == "true")
parseDecision (ChooseCard _ choices f) input    = f $ head (filter ((==(lookupCard input)) . typ) choices)
parseDecision (ChooseCards _ choices _ f) input = f $ Maybe.fromJust $ findCards choices (map lookupCard (wordsBy (==',') input))
parseDecision (ChooseEffects _ effects f) input = f (map ((effects!!) . read) (wordsBy (==',') input))
parseDecision (Optional inner other) input      = if input == "" then other else parseDecision inner input

makeDecisionHandler :: IORef.IORef GameRepository -> Snap ()
makeDecisionHandler gamesRepository = do
  id <- getParam "id"
  player <- getParam "player"
  (_,games) <- liftIO $ IORef.readIORef gamesRepository

  let gameId = fst $ Maybe.fromJust $ C8.readInt (Maybe.fromJust id)
  let playerId = C8.unpack (Maybe.fromJust player)
  let (states,simState) = games Map.! gameId
  let mvar = externalDecision $ states Map.! playerId

  param <- readRequestBody 1000
  let text = LBS.toStrict param

  decision <- liftIO $ takeMVar mvar
  case decision of
    (State _) -> writeBS "no decision to make, game has ended"
    (Decision _ _ dec) -> do
      let (next,simState') = runSim (parseDecision dec $ C8.unpack text) (sim2Gen simState)

      liftIO $ do
        game' <- nextStep (states,combineSimStates simState simState') next
        IORef.modifyIORef' gamesRepository (\(id,games) -> (id,Map.insert gameId game' games))

      nextDecision gamesRepository gameId playerId


-- Simulation

simulationHandler :: Snap ()
simulationHandler =
  do
    num <- getParam "num"
    let numGames = maybe 1000 fst (num >>= C8.readInt)

    cards <- getParam "cards"
    -- TODO make this safe
    let cardNames = C8.split ',' $ Maybe.fromJust cards
    let tableau = map (lookupCard . C8.unpack) cardNames

    gen <- liftIO $ newStdGen
    let stats = evalSim (runSimulations [("Alice",bigSmithy "Alice"), ("Bob",doubleJack "Bob")]
                                        tableau
                                        (emptyStats ["Alice","Bob"])
                                        numGames)
                        gen

    writeHtml $ htmlSimulation tableau stats
