{-# LANGUAGE OverloadedStrings, FlexibleInstances, TupleSections, ScopedTypeVariables #-}
module Main where

import Dominion
import Dominion.Library
import Dominion.Model
import Dominion.Cards
import Dominion.Bots hiding (buys)
import Dominion.Stats
import Dominion.Web.Pages

import Control.Applicative
import Control.Concurrent
import qualified Control.Monad as M
import Control.Monad.IO.Class
import qualified Control.Concurrent.STM as STM
import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server
import qualified Data.Aeson as J
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.Either as Either
import qualified Data.IORef as IORef
import Data.List.Split (wordsBy)
import qualified Data.Maybe as Maybe
import qualified Data.Map.Strict as Map
import qualified Data.Map.Lazy as LMap
import qualified System.Environment as Env
import System.Log.FastLogger
import System.Random (newStdGen)

import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Text.Blaze.Html5 as H

import Dominion.Web.JsonInstances


main :: IO ()
main = Env.getArgs >>= \(cmd:args) ->
  case cmd of
    "run" -> runConsole args
    "tournament" -> runTournament args
    "web" -> runWeb
    _ -> return ()

testTableau :: String
testTableau = "market,library,smithy,cellar,chapel,militia,village,laboratory,witch,jack of all trades"

runConsole :: [String] -> IO ()
runConsole [] = runConsole ["1000", defaultBotId, "Big Money", testTableau]
runConsole [num] = runConsole [num, defaultBotId, "Big Money", testTableau]
runConsole [num, a] = runConsole [num, a, "Big Money", testTableau]
runConsole [num, a, b] = runConsole [num, a, b, testTableau]
runConsole (num:a:b:cards:_) =
  do
    bots <- sequence [(Maybe.fromJust $ lookup a botLibrary) a,
                      (Maybe.fromJust $ lookup b botLibrary) b]
    stats <- runSimulations
              (zip [a,b] bots)
              tableau
              (emptyStats [a,b])
              (read num)
    putStrLn (showStats stats)
  where
    tableau = map (lookupCard . C8.unpack) $ C8.split ',' $ C8.pack cards

roundRobin :: Int -> [String] -> IO ()
roundRobin _ [] = return ()
roundRobin _ [_] = return ()
roundRobin n (a:bots) = do
  M.forM_ bots $ \b -> do
    bots <- sequence [(Maybe.fromJust $ lookup a botLibrary) a,
                      (Maybe.fromJust $ lookup b botLibrary) b]
    stats <- runSimulationsR
              (zip [a,b] bots)
              (emptyStats [a,b])
              n
    putStrLn $ show (statWinRatio stats)
  roundRobin n bots

runTournament :: [String] -> IO ()
runTournament [] = error "Need to call tournament with a list of bots"
runTournament (n:bots) = roundRobin (read n) bots

runWeb :: IO ()
runWeb = do
  logset <- newStdoutLoggerSet 1000
  gamesRepository <- IORef.newIORef (0,Map.empty)
  quickHttpServe (site logset gamesRepository)

homePage :: Snap ()
homePage = writeHtml htmlHomePage

site :: LoggerSet -> IORef.IORef GameRepository -> Snap ()
site logset gamesRepository =
    ifTop homePage <|>
    route [ ("simulation", method GET $ simulationHandler),
            ("game/start", method POST $ startGameHandler logset gamesRepository),
            ("game/play", method GET $ setupGameHandler),
            ("game/join", method GET $ joinGameHandler gamesRepository),
            ("game/:id/join", method POST $ doJoinGameHandler logset gamesRepository),
            ("game/suggested", method GET $ suggestedTableauHandler),
            ("game/:id/decision/:player", method GET $ decisionHandler logset gamesRepository),
            ("game/:id/decision/:player", method POST $ makeDecisionHandler logset gamesRepository)
          ] <|>
    dir "static" (serveDirectory "static")


writeHtml :: H.Html -> Snap ()
writeHtml = writeLBS . renderHtml


-- Game API

data DecisionState = GameEnded GameState | DecisionPending GameState (Decision (SimulationT GameState)) (SimulationT GameState -> IO Game)

data PlayerState = Bot (Bot GameState) | External (MVar DecisionState)
type Game = Map.Map PlayerId PlayerState

instance Show PlayerState where
  show (Bot _) = "Bot"
  show (External _) = "External"

type GameRepository = (Int,Map.Map Int (Either (STM.TVar StartGameReq) Game))


gameReadyToStart :: StartGameReq -> Bool
gameReadyToStart = not . any (==OpenSlot) . gamePlayers

addPlayerInSlot :: String -> StartGameReq -> StartGameReq
addPlayerInSlot name req = req { gamePlayers = iter (gamePlayers req) }
  where
    iter [] = []
    iter (OpenSlot:xs) = (HumanPlayer name):xs
    iter (x:xs) = x:iter xs

externalDecision :: PlayerState -> MVar DecisionState
externalDecision (External mvar) = mvar
externalDecision (Bot _) = error "Called externalDecision on internal bot"

playerUpdateState :: PlayerState -> GameState -> IO ()
playerUpdateState (Bot _) _ = return ()
playerUpdateState (External mvar) state = putMVar mvar (GameEnded state)

playerDecision :: Game -> PlayerState -> PlayerId -> GameState -> Decision (SimulationT GameState) -> (Game -> SimulationT GameState -> IO Game) -> IO Game
playerDecision game (Bot bot) _ state decision cont = do
  sim <- bot state decision
  cont game sim

playerDecision game (External mvar) _ state decision cont =
  putMVar mvar (DecisionPending state decision (cont game)) >> return game

launchWebGame :: StartGameReq -> IO Game
launchWebGame req = do
  let (players,cards,gameTyp) = (gamePlayers req, gameCards req, gameType req)
  let tableau = map lookupCard cards
  let initial = (mkGame gameTyp (map playerDefName players) tableau)

  gen <- newStdGen
  mvars <- sequence $ map mkMVar players
  launchGame decisionHandler finishHandler initial (seedSim gen) (Map.fromList mvars)

  where
    mkMVar OpenSlot = undefined
    mkMVar (HumanPlayer name) = newEmptyMVar >>= \mv -> return (name, External mv)
    mkMVar (BotPlayer name botid) = do
      bot <- maybe ((Maybe.fromJust $ lookup defaultBotId botLibrary) name)
                   ($ name)
                   (lookup botid botLibrary)
      return (name, Bot bot)

    decisionHandler game pid state dec cont =
      playerDecision game (game Map.! pid) pid state dec cont

    finishHandler game state = do
      _ <- sequence (map (\p -> playerUpdateState p state) $ Map.elems game)
      return game


setupGameHandler :: Snap ()
setupGameHandler = writeHtml htmlSetupGame

suggestedTableauHandler :: Snap ()
suggestedTableauHandler = (writeHtml . htmlSuggestedTableaus) suggestedTableaus

startGameHandler :: LoggerSet -> IORef.IORef GameRepository -> Snap ()
startGameHandler logset gamesRepository = do
  body <- readRequestBody 10000
  let req = J.decode body :: Maybe StartGameReq
  let startGameReq = Maybe.fromMaybe
                      StartGameReq { gamePlayers = [HumanPlayer "Alice", BotPlayer "Bob" defaultBotId],
                                     gameCards = defaultTableau,
                                     gameType = StandardGame }
                      req

  id <- liftIO $ do
    (nextId,games) <- IORef.readIORef gamesRepository

    if gameReadyToStart startGameReq
      then do
        game <- launchWebGame startGameReq
        IORef.writeIORef gamesRepository (nextId+1, Map.insert nextId (Right game) games)
        pushLogStr logset (toLogStr ("Created & started new game: " ++ show nextId ++ "\n"))
      else do
        tvar <- STM.newTVarIO startGameReq
        IORef.writeIORef gamesRepository (nextId+1, Map.insert nextId (Left tvar) games)
        pushLogStr logset (toLogStr ("Created new game: " ++ show nextId ++ ", waiting for additional players\n"))

    return nextId

  writeBS $ C8.pack $ show id
  where
    defaultTableau = ["market", "library", "smithy", "cellar", "chapel", "militia",
                      "village", "laboratory", "witch", "jack of all trades"]

joinGameHandler :: IORef.IORef GameRepository -> Snap ()
joinGameHandler repo = do
  open <- liftIO $ do
    (_,games) <- IORef.readIORef repo
    let reqs = Either.lefts $ map (\(id,v) -> either (Left . (id,)) Right v) $ Map.toList games
    STM.atomically $ sequence $ map (\(id,v) -> STM.readTVar v >>= \req -> return (id,req)) reqs

  writeHtml $ htmlJoinGame open

doJoinGameHandler :: LoggerSet -> IORef.IORef GameRepository -> Snap ()
doJoinGameHandler logset repo = do
  id <- getParam "id"
  let gameId = fst $ Maybe.fromJust $ C8.readInt (Maybe.fromJust id)

  player <- readRequestBody 100
  let playerId = LC8.unpack player

  (nextId,games) <- liftIO $ IORef.readIORef repo

  case Map.lookup gameId games of
    Just (Left tvar) -> do
      req <- liftIO $ STM.atomically (STM.readTVar tvar
                                      >>= (STM.writeTVar tvar . addPlayerInSlot playerId)
                                      >> STM.readTVar tvar)

      M.when (gameReadyToStart req) $ liftIO $ do
        game <- launchWebGame req
        IORef.writeIORef repo (nextId, Map.insert gameId (Right game) games)
        pushLogStr logset (toLogStr ("Started new game: " ++ show gameId ++ "\n"))

      writeBS "{'status':'success'}"

    _ -> writeBS "{'status':'failed'}"


nextDecision :: IORef.IORef GameRepository -> Int -> PlayerId -> Snap ()
nextDecision gamesRepository gameId player = do
  format <- getParam "format"
  let useHtml = maybe False (=="html") format
  let formatHandler = if useHtml then (renderHtml . (htmlDecision player)) else J.encode

  (_,games) <- liftIO $ IORef.readIORef gamesRepository

  case Map.lookup gameId games of
    Just (Right states) -> do
      let mvar = externalDecision $ states Map.! player

      decision <- liftIO $ readMVar mvar
      case decision of
        (GameEnded state) -> if useHtml
                             then writeHtml (htmlFinished state
                                                          (collectStats (emptyStats (Map.keys (players state))) state))
                             else writeBS "{'status':'finished'}"
        (DecisionPending state dec _) -> writeLBS $ formatHandler (visibleState player state, dec)

    Just (Left _) -> writeHtml $ htmlWaitingForPlayers

    _ -> writeHtml $ htmlError "No such game."


decisionHandler :: LoggerSet -> IORef.IORef GameRepository -> Snap ()
decisionHandler _ gamesRepository = do
  id <- getParam "id"
  player <- getParam "player"

  let gameId = fst $ Maybe.fromJust $ C8.readInt (Maybe.fromJust id)
  let playerId = C8.unpack (Maybe.fromJust player)

  nextDecision gamesRepository gameId playerId

parseDecision :: Decision (SimulationT GameState) -> String -> SimulationT GameState
parseDecision (ChooseToUse _ f) input           = f (input == "true")
parseDecision (ChooseNumber _ _ f) input        = f $ fst $ Maybe.fromJust $ C8.readInt $ C8.pack input
parseDecision (ChooseToReact _ _ f) input       = f (input == "true")
parseDecision (ChooseCard _ choices f) input    = f $ head (filter ((==(lookupCard input)) . typ) choices)
parseDecision (ChooseCards _ choices _ f) input = f $ Maybe.fromJust $ findCards choices (map lookupCard (wordsBy (==',') input))
parseDecision (ChooseEffects _ effects f) input = f (map ((effects!!) . read) (wordsBy (==',') input))
parseDecision (Optional inner other) input      = if input == "" then other else parseDecision inner input

makeDecisionHandler :: LoggerSet -> IORef.IORef GameRepository -> Snap ()
makeDecisionHandler logset gamesRepository = do
  id <- getParam "id"
  player <- getParam "player"
  (_,games) <- liftIO $ IORef.readIORef gamesRepository

  let gameId = fst $ Maybe.fromJust $ C8.readInt (Maybe.fromJust id)
  let playerId = C8.unpack (Maybe.fromJust player)

  liftIO $ pushLogStr logset (toLogStr ("Player " ++ playerId ++ " making decision in game " ++ show gameId ++ "\n"))

  let states = either undefined Prelude.id $ games Map.! gameId
  let mvar = externalDecision $ states Map.! playerId

  param <- readRequestBody 1000
  let text = LBS.toStrict param

  decision <- liftIO $ takeMVar mvar

  case decision of
    (GameEnded _) -> writeBS "no decision to make, game has ended"
    (DecisionPending _ dec cont) -> do
      let next = parseDecision dec $ C8.unpack text
      _ <- liftIO $ cont next
      nextDecision gamesRepository gameId playerId


-- Simulation

-- TODO make all of this safer
simulationHandler :: Snap ()
simulationHandler =
  do
    num <- getParam "num"
    let numGames = maybe 1000 fst (num >>= C8.readInt)

    params <- getParams
    let cardNames = Maybe.fromMaybe (C8.split ',' $ C8.pack testTableau)
                                    (LMap.lookup "cards" params)

    let tableau = map (lookupCard . C8.unpack) cardNames

    [b1, b2] <- sequence $ map getParam ["bot1", "bot2"]
    let bot1 = maybe "Double Jack" C8.unpack b1
    let bot2 = maybe "Big Money" C8.unpack b2
    let p1 = bot1
    -- in case we run a bot against itself
    let p2 = if bot1 == bot2 then bot2 ++ "2" else bot2

    ai1 <- liftIO $ (Maybe.fromJust $ lookup bot1 botLibrary) p1
    ai2 <- liftIO $ (Maybe.fromJust $ lookup bot2 botLibrary) p2

    requestType <- getParam "submit"
    let sampleGame = "Sample Game" == Maybe.fromMaybe "Go" requestType

    if sampleGame
      then do
        state <- liftIO $ runSampleGame [(p1, ai1), (p2, ai2)] tableau
        writeHtml $ htmlSampleGame state tableau [bot1,bot2]

      else do
        stats <- liftIO $ runSimulations [(p1, ai1), (p2, ai2)]
                                          tableau
                                          (emptyStats [p1,p2])
                                          numGames

        writeHtml $ htmlSimulation tableau stats [bot1,bot2]
