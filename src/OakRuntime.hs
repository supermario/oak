{-# LANGUAGE StandaloneDeriving #-}

module OakRuntime where

import SlaveThread as ST
import Control.Concurrent.Chan.Unagi
import Control.Concurrent.STM
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad      (forever)

import qualified Data.Cache as Cache

import qualified Hilt
import qualified Hilt.Server
import qualified Hilt.SocketServer as SocketServer

import qualified App
import Oak

deriving instance Show App.Msg

type ServiceCache = Cache.Cache ServiceKinds Service

run :: IO ()
run =
  Hilt.manage $
    Hilt.program $ do
      let app = App.main

      tModel <- newTVarIO $ init_ app
      (chanW,chanR) <- newChan

      c <- Cache.newCache Nothing :: IO ServiceCache

      -- Print the initial view to screen
      putStrLn $ view_ app $ init_ app

      -- Boot any services required by subscriptions
      mapM_
        (\s -> case s of
          SubKeypress onKeypress ->
            serviceOrCreate c Console $ do
              Cache.insert c Console $ Service NoHandle Console

              _ <- forkIO $ forever $ do
                x <- getChar
                writeChan chanW $ onKeypress [x]

              return ()

          SubWebsocket path onJoined onReceive ->
            serviceOrCreate c WebSocket $ do
              service <- subWebsocket path onJoined onReceive chanW

              Cache.insert c WebSocket service

              case handle service of
                SocketHandle socketHandle -> do
                  _ <- forkIO $ Hilt.Server.runWebsocket socketHandle
                  return ()

                NoHandle -> putStrLn "CmdSocketBroadcast got NoHandle?!"
        )
        (subscriptions_ app)

      -- Create the worker for the msg channel
      worker chanR (\msg -> do

          -- Atomically retrieve, update and replace our model
          (model, cmd) <- atomically $ do
            model <- readTVar tModel
            let (newModel, cmd) = update_ app model msg
            writeTVar tModel newModel
            return (newModel, cmd)

          -- Display our current state
          putStrLn $ view_ app model

          -- Execute any commands
          runCmd c cmd
        )


runCmd :: ServiceCache -> Cmd msg -> IO ()
runCmd c cmd =
  case cmd of
    CmdSocketSend socketId text ->
      withSocketHandle c (\h -> SocketServer.send h socketId text)

    CmdSocketBroadcast text ->
      withSocketHandle c (`SocketServer.broadcast` text)

    CmdDelay milliseconds cmd_ -> do
      sleep milliseconds
      runCmd c cmd_

    CmdNone -> return ()

    -- These exist for reference but are TBC
    -- import System.Exit        (exitSuccess, exitFailure)
    -- CmdExit -> exitSuccess
    --
    -- CmdDie  -> exitFailure

serviceOrCreate :: ServiceCache -> ServiceKinds -> IO () -> IO ()
serviceOrCreate c serviceKind create = do
  serviceM <- Cache.lookup c serviceKind
  case serviceM of
    Just _ -> return ()
    Nothing -> create


withSocketHandle :: ServiceCache -> (SocketServer.Handle -> IO ()) -> IO ()
withSocketHandle c f = do
  socketHandleM <- getSocketHandle c
  case socketHandleM of
    Just socketHandle -> f socketHandle
    Nothing -> putStrLn "[ERROR] Could not find SocketServer handle"


getSocketHandle :: ServiceCache -> IO (Maybe SocketServer.Handle)
getSocketHandle c = do
  serviceLookup <- Cache.lookup c WebSocket
  case serviceLookup of
    Just service ->
      case handle service of
        SocketHandle socketHandle -> return $ Just socketHandle
        NoHandle -> return Nothing

    Nothing -> return Nothing


subWebsocket :: String -> WsJoined msg -> WsReceive msg -> InChan msg -> IO Service
subWebsocket _ onJoinedMsg onReceiveMsg chanW = do
  let
      onJoined :: SocketServer.OnJoined
      onJoined clientId clientCount = do
        writeChan chanW $ onJoinedMsg clientId  clientCount
        return Nothing

      onReceive :: SocketServer.OnReceive
      onReceive clientId text =
        writeChan chanW $ onReceiveMsg clientId text

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


sleep :: Int -> IO ()
sleep milliseconds = threadDelay (milliseconds * 1000)
