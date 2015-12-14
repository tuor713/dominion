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
    route [ ("simulation", simulationHandler),
            ("game/start", method POST $ startGameHandler gamesRepository),
            ("game/play", setupGameHandler),
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

data StartGameReq = StartGameReq { gamePlayers :: [String], gameCards :: [String] }
  deriving (Eq,Show)

instance J.FromJSON StartGameReq where
  parseJSON (J.Object v) = StartGameReq <$>
    v J..: "players" <*>
    v J..: "cards"
  parseJSON _ = empty

startGameHandler :: IORef.IORef GameRepository -> Snap ()
startGameHandler gamesRepository = do
  body <- readRequestBody 10000
  let req = J.decode body :: Maybe StartGameReq
  let (players,cards) = maybe (["Alice","Bob"],defaultTableau) (\req -> (gamePlayers req, gameCards req)) req
  let tableau = map lookupCard cards

  id <- liftIO $ do
    (nextId,games) <- IORef.readIORef gamesRepository
    gen <- newStdGen
    let (newGame,simState) = runSim (mkGame StandardGame players tableau) gen

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

instance J.ToJSON Location where
  toJSON Supply = J.toJSON [J.String "supply"]
  toJSON (Hand p) = J.toJSON [J.String "hand" , jString p]
  toJSON (Discard p) = J.toJSON [J.String "discardPile", jString p]
  toJSON Trash = J.toJSON [J.String "trash"]
  toJSON (TopOfDeck p) = J.toJSON [J.String "topOfDeck", jString p]
  toJSON InPlay = J.toJSON [J.String "inPlay"]
  toJSON (Mat p mat) = J.toJSON [J.String "mat", jString p, jString (show mat)]

instance J.ToJSON Effect where
  toJSON (EffectPlusCards no) = J.toJSON [J.String "plusCards", J.toJSON no]
  toJSON (EffectPlusActions no) = J.toJSON [J.String "plusActions", J.toJSON no]
  toJSON (EffectPlusBuys no) = J.toJSON [J.String "plusBuys", J.toJSON no]
  toJSON (EffectPlusMoney no) = J.toJSON [J.String "plusMoney", J.toJSON no]
  toJSON (EffectDiscardNo no) = J.toJSON [J.String "discardNo", J.toJSON no]
  toJSON (EffectTrashNo no) = J.toJSON [J.String "trashNo", J.toJSON no]
  toJSON (EffectDiscard card from) = J.toJSON [J.String "discard", J.toJSON card, J.toJSON from]
  toJSON (EffectBuy card) = J.toJSON [J.String "buy", J.toJSON card]
  toJSON (EffectGain card from to) = J.toJSON [J.String "gain", J.toJSON card, J.toJSON from, J.toJSON to]
  toJSON (EffectPass card from to) = J.toJSON [J.String "pass", J.toJSON card, J.toJSON from, J.toJSON to]
  toJSON (EffectPut card from to) = J.toJSON [J.String "put", J.toJSON card, J.toJSON from, J.toJSON to]
  toJSON (EffectTrash card from) = J.toJSON [J.String "trash", J.toJSON card, J.toJSON from]
  toJSON (EffectReveal card) = J.toJSON [J.String "reveal", J.toJSON card]
  toJSON (EffectPlayAction card) = J.toJSON [J.String "playAction", J.toJSON card]
  toJSON (EffectPlayCopy card) = J.toJSON [J.String "playCopy", J.toJSON card]
  toJSON (EffectPlayTreasure card) = J.toJSON [J.String "playTreasure", J.toJSON card]
  toJSON (SpecialEffect card) = J.toJSON [J.String "useAbility", J.toJSON card]

instance J.ToJSON Decision where
  toJSON (ChooseToUse effect _) = J.object ["type"   J..= J.String "use",
                                            "effect" J..= T.pack (show effect)]

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
