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
import qualified Data.Map.Lazy as LMap
import qualified System.Environment as Env
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
type Game = (Map.Map PlayerId PlayerState, SimulationState, [GameState])

type GameRepository = (Int,Map.Map Int Game)

externalDecision :: PlayerState -> MVar GameStep
externalDecision (External mvar) = mvar
externalDecision (Bot _) = error "Called externalDecision on internal bot"

playerUpdateState :: PlayerState -> GameState -> IO ()
playerUpdateState (Bot _) _ = return ()
playerUpdateState (External mvar) state = putMVar mvar (State state)

playerDecision :: Game -> PlayerState -> PlayerId -> GameState -> Decision -> IO (Maybe (Game,GameStep))
playerDecision (states,simState,stateLog) (Bot bot) _ state decision = do
  sim <- bot state decision
  let (next,simState') = runSim sim (sim2Gen simState)
  return $ Just ((states,combineSimStates simState simState', stateLog), next)

playerDecision _ (External mvar) id state decision =
  putMVar mvar (Decision id state decision) >> return Nothing

playerStates :: Game -> Map.Map PlayerId PlayerState
playerStates (states,_,_) = states

nextStep :: Game -> GameStep -> IO Game
nextStep game@(states,simState,stateLog) (State state)
  | finished state = sequence (map (\p -> playerUpdateState p state) $ Map.elems $ playerStates game)
                     >> return (states, simState, reverse (state:stateLog))
  | otherwise = do
    let (next,simState') = runSim (playTurn (activePlayerId state) state) (sim2Gen simState)
    nextStep (states, combineSimStates simState simState', state:stateLog) next

nextStep game (Decision p state dec) = do
  next <- playerDecision game ((playerStates game) Map.! p) p state dec
  case next of
    Just (game',step) -> nextStep game' step
    Nothing -> return game


setupGameHandler :: Snap ()
setupGameHandler = writeHtml htmlSetupGame

suggestedTableauHandler :: Snap ()
suggestedTableauHandler = (writeHtml . htmlSuggestedTableaus) suggestedTableaus


startGameHandler :: IORef.IORef GameRepository -> Snap ()
startGameHandler gamesRepository = do
  body <- readRequestBody 10000
  let req = J.decode body :: Maybe StartGameReq
  let (players,cards,gameTyp) = maybe ([HumanPlayer "Alice", BotPlayer "Bob" defaultBotId], defaultTableau, StandardGame)
                                      (\req -> (gamePlayers req, gameCards req, gameType req)) req
  let tableau = map lookupCard cards

  id <- liftIO $ do
    (nextId,games) <- IORef.readIORef gamesRepository
    gen <- newStdGen
    let (newGame,simState) = runSim (mkGame gameTyp (map playerDefName players) tableau) gen

    mvars <- sequence $ map mkMVar players

    game <- nextStep (Map.fromList mvars,simState,[]) (State newGame)
    IORef.writeIORef gamesRepository (nextId+1, Map.insert nextId game games)
    return nextId
  writeBS $ C8.pack $ show id
  where
    defaultTableau = ["market", "library", "smithy", "cellar", "chapel", "militia",
                      "village", "laboratory", "witch", "jack of all trades"]
    mkMVar (HumanPlayer name) = newEmptyMVar >>= \mv -> return (name, External mv)
    mkMVar (BotPlayer name botid) = do
      bot <- maybe ((Maybe.fromJust $ lookup defaultBotId botLibrary) name)
                   ($ name)
                   (lookup botid botLibrary)
      return (name, Bot bot)


nextDecision :: IORef.IORef GameRepository -> Int -> PlayerId -> Snap ()
nextDecision gamesRepository gameId player = do
  format <- getParam "format"
  let useHtml = maybe False (=="html") format
  let formatHandler = if useHtml then (renderHtml . (htmlDecision player)) else J.encode

  (_,games) <- liftIO $ IORef.readIORef gamesRepository

  let (states,simState,stateLog) = games Map.! gameId
  let mvar = externalDecision $ states Map.! player

  decision <- liftIO $ readMVar mvar
  case decision of
    (State state) -> if useHtml
                     then writeHtml (htmlFinished state
                                                  (collectStats (emptyStats (Map.keys (players state))) stateLog)
                                                  (sim2Infos simState))
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
  let (states,simState,stateLog) = games Map.! gameId
  let mvar = externalDecision $ states Map.! playerId

  param <- readRequestBody 1000
  let text = LBS.toStrict param

  decision <- liftIO $ takeMVar mvar
  case decision of
    (State _) -> writeBS "no decision to make, game has ended"
    (Decision _ _ dec) -> do
      let (next,simState') = runSim (parseDecision dec $ C8.unpack text) (sim2Gen simState)

      liftIO $ do
        game' <- nextStep (states,combineSimStates simState simState',stateLog) next
        IORef.modifyIORef' gamesRepository (\(id,games) -> (id,Map.insert gameId game' games))

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
        (state,infos) <- liftIO $ runSampleGame [(p1, ai1), (p2, ai2)] tableau
        writeHtml $ htmlSampleGame state tableau infos [bot1,bot2]

      else do
        stats <- liftIO $ runSimulations [(p1, ai1), (p2, ai2)]
                                          tableau
                                          (emptyStats [p1,p2])
                                          numGames

        writeHtml $ htmlSimulation tableau stats [bot1,bot2]
