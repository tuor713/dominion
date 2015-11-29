{-# LANGUAGE OverloadedStrings #-}
module Main where

import Dominion
import Dominion.Model
import Dominion.Cards
import Dominion.Bots

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson as J
import qualified Data.Maybe as Maybe
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import System.Log.FastLogger
import qualified System.Environment as Env
import System.Random (StdGen, mkStdGen, randomR, newStdGen)

runConsole :: [String] -> IO ()
runConsole (num:_) =
  do
    gen <- newStdGen
    let games = evalSim (runSimulations [("Alice",bigSmithy "Alice"), ("Bob",doubleJack "Bob")] tableau (read num)) gen
    putStrLn (stats games)
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
  "[" `C8.append`
  (C8.intercalate "," $ map (\(x,y) -> C8.pack ("{name:"++ show x ++ ", value:" ++ show y ++ "}")) xs)
  `C8.append` "]"

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
    let games = evalSim (runSimulations [("Alice",bigSmithy "Alice"), ("Bob",doubleJack "Bob")] tableau numGames) gen
    let gameLengths = L.sortOn fst $ frequencies (map (turnNo . last) games)

    writeBS $ "<html><head>"
              `C8.append` "<link rel=\"stylesheet\" href=\"/static/site.css\">"
              `C8.append` "<script src=\"//d3js.org/d3.v3.min.js\" charset=\"utf-8\"></script>"
              `C8.append` "<script src=\"/static/js/graph.js\" charset=\"utf-8\"></script>"
              `C8.append` "</head><html>"
              `C8.append` "<h3>Stats</h3>"
              `C8.append` "<pre>"
              `C8.append` C8.pack (stats games)
              `C8.append` "</pre>"
              `C8.append` "<h3>Winners</h3><svg id=\"winners\" class=\"chart\" />"
              `C8.append` "<h3>Turns per Game</h3><svg id=\"turns\" class=\"chart\" />"
              `C8.append` "<script>"

              `C8.append` "barChart(\"#winners\",300,200,percentageData("
              `C8.append` dataToJavaScriptArray (winRatio games)
              `C8.append` "));"

              `C8.append` "barChart(\"#turns\",600,400,percentageData("
              `C8.append` dataToJavaScriptArray gameLengths
              `C8.append` "));"

              `C8.append` "</script>"
              `C8.append` "</body></html>"


echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
          writeBS param

-- end
