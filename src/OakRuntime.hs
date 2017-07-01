{-# LANGUAGE StandaloneDeriving #-}
-- Trying out the Elm architecture with Hilt

module OakRuntime where

import Prelude hiding
  ((.) -- See `Experimental` in Oak.hs
  ,catch)

import SlaveThread as ST
import Control.Concurrent.Chan.Unagi
import Control.Concurrent.STM
import Text.Read          (readMaybe)
import Control.Concurrent (threadDelay, forkIO)
import Control.Exception  (catch, evaluate, ErrorCall)
import Control.Monad      (forever, forM_)
import qualified Hilt

import qualified App
import Oak

deriving instance Show App.Msg

run :: IO ()
run =
  Hilt.manage $
    Hilt.program $ do
      let app = App.main

      tModel <- newTVarIO $ app.init_
      (chanW,chanR) <- newChan

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

          -- Run new cmd
          runCmd chanW cmd
        )

      -- Print the initial view to screen
      putStrLn $ app.view_ $ app.init_

      -- Run our subscriptions, and we're off!
      forever $ mapM_ (>>= writeChan chanW) $ app.subscriptions_


runCmd :: InChan msg -> Cmd msg -> IO ()
runCmd chanW cmd = do
  -- @TODO forkIO will not crash the runtime on unhandled child exceptions
  -- However it won't prevent children creating ghost threads like ST.fork
  -- Is there a balanced alternative?
  let thread = do
        ST.fork $ do
          maybeMsg <- task_ cmd
          forM_ maybeMsg $ writeChan chanW
        return ()

  -- https://wiki.haskell.org/Error_vs._Exception#When_errors_become_exceptions
  catch (evaluate thread) errorHandler
  return ()


errorHandler :: ErrorCall -> IO (IO ())
errorHandler e =
    return $ do
      putStrLn "Exception thrown by task!"
      print (e :: ErrorCall)




worker :: OutChan msg -> (msg -> IO a) -> IO ()
worker chan handler = do
  _ <- ST.fork $ forever $ do
    text <- readChan chan
    handler text
  return ()
