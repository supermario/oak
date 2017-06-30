-- Trying out the Elm architecture with Hilt

module Main where

import qualified Hilt

import SlaveThread as ST
import Control.Concurrent.Chan.Unagi
import Control.Concurrent.STM
import Text.Read (readMaybe)
import Control.Concurrent    (threadDelay)
import Control.Monad         (forever)

import Prelude hiding (init)

import qualified App
import Oak

main :: IO ()
main =
  Hilt.manage $
    Hilt.program $ do
      tModel <- newTVarIO $ Oak.init_ App.main
      (chanW,chanR) <- newChan

      putStrLn $ Oak.view_ App.main $ Oak.init_ App.main

      -- Create a worker listening to the message channel
      worker chanR (\msg -> do
          output <- atomically $ do
            model <- readTVar tModel
            let newModel = App.update model msg
            writeTVar tModel newModel
            return $ App.view newModel

          putStrLn output
        )

      let subs = Oak.subscriptions_ App.main
      forever $ mapM_ (>>= writeChan chanW) subs

      -- Simulate delay + event
      -- threadDelay 2000
      -- writeChan chanW Increment
      -- threadDelay 3000
      -- writeChan chanW Increment
      -- writeChan chanW Increment
      -- writeChan chanW Increment
      -- threadDelay 3000
      -- writeChan chanW Decrement

worker :: OutChan t -> (t -> IO a) -> IO ()
worker chan handler = do
  _ <- ST.fork $ forever $ do
    text <- readChan chan
    handler text
  return ()
