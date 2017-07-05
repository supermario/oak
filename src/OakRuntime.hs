{-# LANGUAGE StandaloneDeriving #-}

module OakRuntime where

import Prelude hiding
  ((.)) -- See `Experimental` in Oak.hs

import SlaveThread as ST
import Control.Concurrent.Chan.Unagi
import Control.Concurrent.STM
import Control.Concurrent (forkIO)
import Control.Monad      (forever)

import qualified Data.Cache as Cache

import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai                    (responseLBS)
import Network.HTTP.Types             (status400)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets             (defaultConnectionOptions)

import qualified Hilt
import qualified Hilt.Config as Config
import qualified Hilt.SocketServer as SocketServer

import qualified App
import Oak

deriving instance Show App.Msg

run :: IO ()
run = do
  port <- Config.lookupEnv "PORT" 8081

  Hilt.manage $
    Hilt.program $ do
      let app = App.main

      tModel <- newTVarIO $ app.init_
      (chanW,chanR) <- newChan

      c <- Cache.newCache Nothing :: IO (Cache.Cache ServiceKinds Service)

      mapM_
        (\s -> case s of
          SubKeypress onKeypress -> do
            -- @TODO check for presence first
            Cache.insert c Console $ Service NoHandle Console

            _ <- forkIO $ forever $ do
              x <- getChar
              writeChan chanW $ onKeypress [x]

            return ()

          SubWebsocket path onJoined onReceive -> do
            service <- subWebsocket path onJoined onReceive chanW

            -- @TODO check for presence first
            Cache.insert c WebSocket service

            _ <- forkIO $
              case handle service of
                SocketHandle socketHandle ->
                  let backupApp _ respond = respond $ responseLBS status400 [] "Not a WebSocket request"
                      waiApp = websocketsOr defaultConnectionOptions (SocketServer.app socketHandle) backupApp

                  in Warp.run port waiApp
                NoHandle -> putStrLn "CmdSocketBroadcast got NoHandle?!"

            return ()

        )
        (app.subscriptions_)

      putStrLn "Services done..."

      -- Create a worker listening to the message channel
      worker chanR (\msg -> do

          -- Atomically retrieve, update and replace our model
          (model, cmd) <- atomically $ do
            model <- readTVar tModel
            let (newModel, cmd) = (app.update_) model msg
            writeTVar tModel newModel
            return (newModel, cmd)

          -- Display our current state
          putStrLn $ app.view_ $ model

          case cmd of
            CmdSocketSend socketId text -> do
              serviceLookup <- Cache.lookup c WebSocket
              case serviceLookup of
                Just service ->
                  case handle service of
                    SocketHandle socketHandle -> websocketSend socketId text socketHandle
                    NoHandle -> putStrLn "CmdSocketSend got NoHandle?!"

                Nothing -> putStrLn "CmdSocketSend could not find handle!"

            CmdSocketBroadcast text -> do
              serviceLookup <- Cache.lookup c WebSocket
              case serviceLookup of
                Just service ->
                  case handle service of
                    SocketHandle socketHandle -> websocketBroadcast text socketHandle
                    NoHandle -> putStrLn "CmdSocketBroadcast got NoHandle?!"
                Nothing -> putStrLn "CmdSocketBroadcast could not find handle!"

            CmdNone ->
              putStrLn "CmdNone received."

        )

      -- Print the initial view to screen
      putStrLn $ app.view_ $ app.init_


subWebsocket :: String -> WsJoined msg -> WsReceive msg -> InChan msg -> IO Service
subWebsocket _ onJoinedMsg onReceiveMsg chanW = do
  let
      onJoined :: SocketServer.OnJoined
      onJoined clientCount = do
        -- @TODO fix clientId stub
        writeChan chanW $ onJoinedMsg 123 clientCount
        return Nothing

      onReceive :: SocketServer.OnReceive
      onReceive text =
        -- @TODO fix clientId stub
        writeChan chanW $ onReceiveMsg 123 text

  socketServerH <- SocketServer.loadRaw onJoined onReceive

  return Service
    { handle = SocketHandle socketServerH
    , kind = WebSocket
    }


worker :: OutChan msg -> (msg -> IO a) -> IO ()
worker chan handler = do
  _ <- ST.fork $ forever $ do
    text <- readChan chan
    handler text
  return ()
