{-# LANGUAGE OverloadedStrings #-}
module Main where

import Dominion
import Dominion.Model
import Dominion.Cards
import Dominion.Bots
import Dominion.Stats

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server
import qualified Data.Aeson as J
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LBS
import Data.Char (toLower)
import qualified Data.List as L
import qualified Data.Maybe as Maybe
import qualified Data.Map.Strict as Map
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import System.Log.FastLogger
import qualified System.Environment as Env
import System.Random (StdGen, mkStdGen, randomR, newStdGen)

import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html (toHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Internal (MarkupM(Parent))

runConsole :: [String] -> IO ()
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
  quickHttpServe (site logset)

main :: IO ()
main =
  do
    args <- Env.getArgs
    case (head args) of
      "run" -> runConsole (tail args)
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

site :: LoggerSet -> Snap ()
site logset =
    ifTop (writeBS "Welcome to Dominion Simulation server") <|>
    method POST (path "act" (postHandler logset)) <|>
    route [ ("echo/:echoparam", echoHandler),
            ("simulation", simulationHandler),
            ("version", writeBS (BS.concat ["Running Snap: ", snapServerVersion, "\n"]))
          ] <|>
    dir "static" (serveDirectory "static")

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


echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
          writeBS param

-- end
