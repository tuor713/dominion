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
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson as J
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import System.Log.FastLogger
import qualified System.Environment as Env

{-
main :: IO ()
main = do
  logset <- newStdoutLoggerSet 10000
  quickHttpServe (site logset)
-}

main :: IO ()
main =
  do
    args <- Env.getArgs
    traces <- runSimulations [("Alice",bigSmithy "Alice"), ("Bob",doubleJack "Bob")]
      (map lookupCard ["market", "library", "smithy", "cellar", "chapel", "witch",
                       "village", "laboratory", "festival", "jack of all trades"])
      (case args of
        (x:_) -> read x
        [] -> 10000)
    stats traces

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
    ifTop (writeBS "hello world") <|>
    method POST (path "act" (postHandler logset)) <|>
    route [ ("foo", writeBS "bar")
          , ("echo/:echoparam", echoHandler)
          , ("version", writeBS (BS.concat ["Running Snap: ", snapServerVersion, "\n"]))
          ] <|>
    dir "static" (serveDirectory ".")

echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
          writeBS param

-- end
