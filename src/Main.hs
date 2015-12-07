{-# LANGUAGE OverloadedStrings, FlexibleInstances, TupleSections, ScopedTypeVariables #-}
module Main where

import Dominion
import Dominion.Model
import Dominion.Cards
import Dominion.Bots hiding (buys)
import Dominion.Stats

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server
import qualified Data.Aeson as J
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LBS
import Data.Char (toLower)
import qualified Data.IORef as IORef
import qualified Data.List as L
import Data.List.Split (wordsBy)
import qualified Data.Maybe as Maybe
import qualified Data.Map.Strict as Map
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Vector as V
import System.Log.FastLogger
import qualified System.Environment as Env
import System.Random (newStdGen)

import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html (toHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Internal (MarkupM(Parent))

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

main :: IO ()
main = Env.getArgs >>= \(cmd:args) ->
  case cmd of
    "run" -> runConsole args
    "web" -> runWeb
    _ -> return ()

data Message = Message { message :: T.Text } deriving (Show)

instance J.FromJSON Message where
  parseJSON (J.Object v) = Message <$> (v J..: "message")
  parseJSON _ = mzero

instance J.FromJSON Card where
  parseJSON (J.Object v) =
    do
      s <- v J..: "card"
      return $ lookupCard s
  parseJSON _ = mzero

postHandler :: LoggerSet -> Snap ()
postHandler logset = do
  param <- readRequestBody 1000
  liftIO (pushLogStr logset (toLogStr $ T.concat ["Post called with argument: '", E.decodeUtf8 $ LBS.toStrict param, "'\n"]))
  writeText $ maybe "Card not found:"
                    (\m -> T.concat ["Card cost is ", T.pack $ show (cost m)])
                    (J.decode param :: Maybe Card)


homePage :: Snap ()
homePage = writeBS (BS.concat ["Welcome to Dominion Simulation server\n",
                               "Running Snap: ", snapServerVersion, "\n"])

site :: IORef.IORef GameRepository -> LoggerSet -> Snap ()
site gamesRepository logset =
    ifTop homePage <|>
    method POST (path "act" (postHandler logset)) <|>
    route [ ("simulation", simulationHandler),
            ("game/start", startGameHandler gamesRepository),
            ("game/:id/decision/:player", method GET $ decisionHandler gamesRepository),
            ("game/:id/decision/:player", method POST $ makeDecisionHandler gamesRepository)
          ] <|>
    dir "static" (serveDirectory "static")


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


startGameHandler :: IORef.IORef GameRepository -> Snap ()
startGameHandler gamesRepository = do
  id <- liftIO $ do
    (nextId,games) <- IORef.readIORef gamesRepository
    gen <- newStdGen
    let players = ["Alice","Bob"]
    let (newGame,simState) = runSim (mkGame StandardGame players tableau) gen

    aliceVar <- newEmptyMVar
    let mvars = (Map.fromList
                  [("Alice", External aliceVar), ("Bob", Bot $ aiBot (betterBigMoney "Bob"))])

    game <- nextStep (mvars,simState) (State newGame)
    IORef.writeIORef gamesRepository (nextId+1, Map.insert nextId game games)
    return nextId
  writeBS $ C8.pack $ show id
  where
    tableau = map lookupCard ["market", "library", "smithy", "cellar", "chapel", "militia",
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


input :: H.Html -> H.Html
input = Parent "input" "<input" "</input>"


decisionHtml :: Decision -> H.Html
decisionHtml (Optional inner _) =
  H.div $ do
    decisionHtml inner
    H.button H.! A.class_ "choice-button" H.! A.onclick "pass();" $ "Pass"

decisionHtml (YesNo typ card _) =
  H.div $ do
    toHtml $ (show typ) ++ " of " ++ cardName card
    H.button H.! A.class_ "choice-button" H.! A.onclick "choose('true');" $ "Yes"
    H.button H.! A.class_ "choice-button" H.! A.onclick "choose('false');" $ "No"

decisionHtml (Choice typ choices _) =
  H.div $ do
    toHtml $ "Choose a cards to " ++ show typ ++ ":"
    H.div H.! A.id "choices" $ do
      forM_ choices $ \card -> do
        H.input H.! A.type_ "image"
                H.! A.onclick (fromString ("choose('"++ cardName card ++ "',1,1);"))
                H.! A.style "margin: 5px; width: 100px; height: 159px"
                H.! A.src (fromString (cardImagePath card))


decisionHtml (Choices typ choices (lo,hi) _) =
  H.div $ do
    toHtml $ "Choose between " ++ show lo ++ " and " ++ show hi ++ " cards to " ++ show typ ++ ":"
    H.div H.! A.id "choices" $ do
      forM_ choices $ \card -> do
        H.input H.! A.type_ "image"
                H.! A.name (fromString (cardName card))
                H.! A.class_ "checkbox"
                H.! A.style "margin: 5px; width: 100px; height: 159px"
                H.! A.src (fromString (cardImagePath card))
    if length choices <= hi
      then H.button H.! A.class_ "choice-button"
                    H.! A.onclick (fromString ("choose('"++ L.intercalate "," (map cardName choices) ++ "');"))
                    $ "All"
      else return ()
    H.button H.! A.class_ "choice-button"
             H.! A.onclick (fromString ("choices('choices'," ++ show lo ++ "," ++ show hi ++ ");")) $ "Go"


showCards cards =
  forM_ (L.sortBy (\(c1,_) (c2,_) -> compare (cost c1) (cost c2) `mappend` compare (cardName c1) (cardName c2)) (Map.toList cards))
    $ \(card,num) ->
      H.div H.! A.class_ "imageoverlay" $ do
        H.img H.! A.style "margin: 5px; width: 100px; height: 159px"
              H.! A.src (fromString (cardImagePath card))
        H.h3 H.! A.class_ "overlay" $ toHtml (show num)

cardListToMap :: [Card] -> Map.Map Card Int
cardListToMap cards =
  Map.fromListWith (+) $ map (,1) cards

decisionPage :: PlayerId -> (GameState,[Info],Decision) -> LBS.ByteString
decisionPage _ (state,infos,decision) =
  renderHtml $
    H.html $ do
      H.head $ do
        H.link H.! A.href "/static/site.css" H.! A.rel "stylesheet"
        H.link H.! A.href "/static/libs/semanticui/semantic.min.css" H.! A.rel "stylesheet" H.! A.type_ "text/css"
        H.script H.! A.src "/static/js/jquery-2.1.4.min.js" $ ""
        H.script H.! A.src "/static/libs/semanticui/semantic.min.js" $ ""
        H.script H.! A.src "/static/js/play.js" $ ""
      H.body $ do
        H.div H.! A.class_ "ui grid" $ do
          H.div H.! A.class_ "ten wide column" $ do
            H.section H.! A.class_ "decision" $ do
              H.h3 "Decision"
              decisionHtml decision

            H.section H.! A.class_ "state" $ do
              H.div $ do
                H.h4 "Turn: "
                H.span $ toHtml $ "Turn: " ++ show (turnNo state) ++ ", "
                H.span $ toHtml $ "Actions: " ++ show (actions (turn state)) ++ ", "
                H.span $ toHtml $ "Buys: " ++ show (buys (turn state)) ++ ", "
                H.span $ toHtml $ "Money: " ++ show (money (turn state)) ++ ", "

              H.div $ do
                H.h4 "Tableau:"
                showCards (piles state)

              forM_ (Map.toAscList (players state)) $ \(pid,player) ->
                H.div $ do
                  H.h4 $ toHtml $ "Player - " ++ pid
                  when (not (null (deck player))) $ do
                    H.h5 "Deck: "
                    showCards (cardListToMap (deck player))
                  when (not (null (discardPile player))) $ do
                    H.h5 "Discard: "
                    showCards (cardListToMap (discardPile player))
                  when (not (null (hand player))) $ do
                    H.h5 "Hand: "
                    showCards (cardListToMap (hand player))
                  when (not (null (inPlay player))) $ do
                    H.h5 "InPlay: "
                    showCards (cardListToMap (inPlay player))

          H.div H.! A.class_ "six wide column" $ do
            H.section H.! A.class_ "logs" $ do
              H.h3 "Game Log"
              H.div $ do
                forM_  infos $ \(vis,msg) ->
                  H.div H.! A.class_ "log" $ do
                    H.span H.! A.class_ "player" $ toHtml ("@" ++ show vis ++ " ")
                    H.span $ toHtml msg


nextDecision :: IORef.IORef GameRepository -> Int -> PlayerId -> Snap ()
nextDecision gamesRepository gameId player = do
  format <- getParam "format"
  let formatHandler = maybe J.encode (\f -> if f == "html" then decisionPage player else J.encode) format

  (_,games) <- liftIO $ IORef.readIORef gamesRepository

  let (states,simState) = games Map.! gameId
  let mvar = externalDecision $ states Map.! player


  decision <- liftIO $ readMVar mvar
  case decision of
    (State _) -> writeBS "Game has finished"
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

dataToJavaScriptArray :: (Show a, Show b) => [(a,b)] -> String
dataToJavaScriptArray xs =
  C8.unpack $
    "[" `C8.append`
    (C8.intercalate "," $ map (\(x,y) -> C8.pack ("{name:"++ show x ++ ", value:" ++ show y ++ "}")) xs)
    `C8.append` "]"

cardImagePath :: Card -> String
cardImagePath card = "/static/images/cards/" ++ map (\c -> if c == ' ' then '_' else toLower c) (cardName card) ++ ".jpeg"

svg :: H.Html -> H.Html
svg = Parent "svg" "<svg" "</svg>"

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
    let gameLengths = statTurnsPerGame stats

    writeLBS $ renderHtml $
      H.html $ do
        H.head $ do
          H.link H.! A.href "/static/site.css" H.! A.rel "stylesheet"
          H.script H.! A.src "/static/js/d3.v3.min.js" H.! A.charset "utf-8" $ ""
          H.script H.! A.src "/static/js/graph.js" H.! A.charset "utf-8" $ ""

        H.body $ do
          H.h3 "Tableau"

          H.div $ do
            forM_ (L.sortBy (\c1 c2 -> compare (cost c1) (cost c2) `mappend` compare (cardName c1) (cardName c2)) tableau) $ \card ->
              H.img H.! A.style "margin: 5px; width: 150px; height: 238px"
                    H.! A.src (fromString (cardImagePath card))

          H.h3 "Stats"
          H.pre $ toHtml $ showStats stats

          H.h3 "Winners"
          svg H.! A.id "winners" H.! A.class_ "chart" $ ""

          H.h3 "Turns per Game"
          svg H.! A.id "turns" H.! A.class_ "chart" $ ""

          H.h3 "Average Victory Points per Turn"
          svg H.! A.id "avgVictory" H.! A.class_ "chart" $ ""

          H.script $ fromString
                     ("barChart(\"#winners\",300,200,percentageData(" ++
                      dataToJavaScriptArray (statWinRatio stats) ++
                      "));\n" ++

                      "barChart(\"#turns\",600,400,percentageData(" ++
                      dataToJavaScriptArray gameLengths ++
                      "));\n" ++

                      "scatterPlot(\"#avgVictory\",600,400,"++
                      dataToJavaScriptArray ((statAvgVictoryPerTurn stats) Map.! "Alice") ++ "," ++
                      dataToJavaScriptArray ((statAvgVictoryPerTurn stats) Map.! "Bob") ++
                      ");\n"
                      )
