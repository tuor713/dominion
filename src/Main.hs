{-# LANGUAGE OverloadedStrings #-}
module Main where

import Dominion
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

main :: IO ()
main = do
  logset <- newStdoutLoggerSet 10000
  quickHttpServe (site logset)

data Message = Message { message :: T.Text } deriving (Show)

instance J.FromJSON Message where
  parseJSON (J.Object v) = Message <$> (v J..: "message")
  parseJSON _ = mzero

instance J.FromJSON Card where
  parseJSON (J.Object v) = read <$> (v J..: "card")
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
