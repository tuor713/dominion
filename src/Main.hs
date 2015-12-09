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
import System.Log.FastLogger
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
  logset <- newStdoutLoggerSet 10000
  gamesRepository <- IORef.newIORef (0,Map.empty)
  quickHttpServe (site gamesRepository logset)

homePage :: Snap ()
homePage = writeHtml htmlHomePage

site :: IORef.IORef GameRepository -> LoggerSet -> Snap ()
site gamesRepository logset =
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
                      (\name -> (name, Bot $ aiBot (betterBigMoney name)))
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

instance J.ToJSON Decision where
  toJSON (YesNo typ card _) = J.object ["type"   J..= J.String "yesNo",
                                        "card"   J..= J.toJSON (cardName card),
                                        "action" J..= T.pack (show typ)]

  toJSON (Choice typ choices _) = J.object ["type"   J..= J.String "choice",
                                            "cards"  J..= J.Array (V.fromList (map (J.toJSON . cardName) choices)),
                                            "action" J..= T.pack (show typ)]

  toJSON (Choices typ choices (lo,hi) _) = J.object ["type"   J..= J.String "choices",
                                                     "cards"  J..= J.Array (V.fromList (map (J.toJSON . cardName) choices)),
                                                     "min"    J..= J.toJSON lo,
                                                     "max"    J..= J.toJSON hi,
                                                     "action" J..= J.toJSON (show typ)
                                                     ]

  toJSON (Optional inner _) = J.object ["type"     J..= J.String "optional",
                                        "decision" J..= J.toJSON inner]

instance J.ToJSON Player where
  toJSON player = J.object $ zip ["hand","deck","inPlay","discard"] $ map (jMap . cardMap . ($ player)) [hand, deck, inPlay, discardPile]
    where
      cardMap :: [Card] -> Map.Map String Int
      cardMap cards = Map.mapKeys cardName $ Map.fromListWith (+) (map (,1) cards)

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
parseDecision (YesNo _ _ f) input = f (input == "true")
parseDecision (Choice _ _ f) input = f (lookupCard input)
parseDecision (Choices _ _ _ f) input = f (map lookupCard (wordsBy (==',') input))
parseDecision (Optional inner other) input = if input == "" then other else parseDecision inner input

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
