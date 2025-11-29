{-# LANGUAGE OverloadedStrings #-}

module Main where

import Coffeepot
import Server
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types.Status
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as LBS
import Control.Concurrent.MVar

exampleCoffeepot :: Coffeepot
exampleCoffeepot = Coffeepot
  { state = Idle
  , temperature = 85.0
  , waterLevel = 500
  , coffeeLevel = 100
  , supported = [AllowCategory Milk]
  }

main :: IO ()
main = do
  coffeepotVar <- newMVar exampleCoffeepot
  putStrLn "Starting HTCPCP Server on port 3000..."
  run 3000 $ contentTypeMiddleware $ app coffeepotVar

contentTypeMiddleware :: Middleware
contentTypeMiddleware nextApp req respond = 
  case checkCoffeepotContentType req of
    Left errorResp -> respond errorResp
    Right () -> nextApp req respond

app :: MVar Coffeepot -> Application
app coffeepotVar req respond = do
    let path = pathInfo req
        method = requestMethod req
    case (method, path) of
        ("POST", ["pot-1"]) -> handleBrew coffeepotVar req respond
        ("BREW", ["pot-1"]) -> handleBrew coffeepotVar req respond
        ("GET", ["pot-1"])  -> handleInfo coffeepotVar req respond
        (_, ["teapot-1"])   -> handleTeapot req respond
        _                   -> respond $ responseLBS notFound404 [] "Not Found"


handleBrew :: MVar Coffeepot -> Application
handleBrew coffeepotVar req respond = do
  bodyLBS <- lazyRequestBody req
  let bodyText = TE.decodeUtf8 . LBS.toStrict $ bodyLBS
      additionsValues = lookup "Accept-Additions" (requestHeaders req)
      additions = maybe [] (parseAcceptAdditions . TE.decodeUtf8) additionsValues
      actionText = T.unpack bodyText
  currentPot <- readMVar coffeepotVar
  
  case actionText of
    "start" -> do
      case brewStart currentPot additions of
        Left err -> respond (errorResponse err)
        Right newPot -> do
          _ <- swapMVar coffeepotVar newPot
          respond $ responseLBS ok200 [safeHeader SafeNo] "Brewing started"
          
    "stop" -> do
      case brewStop currentPot of
        Left err -> respond (errorResponse err)
        Right newPot -> do
          _ <- swapMVar coffeepotVar newPot
          respond $ responseLBS ok200 [safeHeader SafeNo] "Brewing stopped"
          
    _ -> respond $ responseLBS badRequest400 [] "Invalid command. Expected 'start' or 'stop'"

handleInfo :: MVar Coffeepot -> Application
handleInfo coffeepotVar _req respond = do
  currentPot <- readMVar coffeepotVar
  let potInfo = T.pack $ show currentPot
  respond $ responseLBS ok200 [safeHeader SafeYes] (LBS.fromStrict . TE.encodeUtf8 $ potInfo)

handleTeapot :: Application
handleTeapot _req respond = do
  respond $ responseLBS imATeapot418 [] "Hey! I'm a teapot"