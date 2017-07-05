module Oak where

import Prelude hiding ((.))

import Control.Concurrent (threadDelay)
-- import System.Exit        (exitSuccess, exitFailure)
import System.IO.Unsafe   (unsafePerformIO)
import Data.Text          (Text)

import Data.Hashable (Hashable, hashWithSalt)
import Hilt.SocketServer


data Program model msg = Program
  { init_ :: model
  , update_ :: model -> msg -> (model, Cmd msg)
  , view_ :: model -> String
  , subscriptions_ :: [Subscription msg]
  }


data Cmd msg
  = CmdSocketSend SocketId Text
  | CmdSocketBroadcast Text
  | CmdNone


data Subscription msg
  = SubKeypress (String -> msg)
  | SubWebsocket String (WsJoined msg) (WsReceive msg)


type SocketId = Int
type WsJoined msg  = (SocketId -> Int -> msg)
type WsReceive msg = (SocketId -> Text -> msg)


data Service = Service
  { handle :: ServiceHandle
  , kind :: ServiceKinds }


data ServiceKinds = WebSocket | Console deriving (Eq, Enum)
instance Hashable ServiceKinds where
  hashWithSalt _ = fromEnum


data ServiceHandle
  = SocketHandle Hilt.SocketServer.Handle
  | NoHandle


cmdNone :: Cmd msg
cmdNone
  = CmdNone


socketSend :: SocketId -> Text -> Cmd msg
socketSend = CmdSocketSend


socketBroadcast :: Text -> Cmd msg
socketBroadcast = CmdSocketBroadcast


-- delay :: Int -> Cmd msg -> Cmd msg
-- delay milliseconds cmd =
--   task $ do
--     sleep milliseconds
--     task_ cmd

-- naughty :: Cmd msg
-- naughty =
--   task $ error "Oops! Error!"

-- asTask :: msg -> Cmd msg
-- asTask msg =
--   task $ return $ Just msg


-- @TODO these commands will do nothing in a regular program, which is good!
-- But what if we want to write a command line utility with Oak?
-- Do we have a different program type that allows these tasks, which would also
-- allow unhandled exceptions and errors to crash the program?
-- exit :: Cmd msg
-- exit =
--   task exitSuccess
--
--
-- die :: Cmd msg
-- die =
--   task exitFailure


keypressListen :: (String -> msg) -> Subscription msg
keypressListen = SubKeypress

websocketListen :: String -> WsJoined msg -> WsReceive msg -> Subscription msg
websocketListen = SubWebsocket


-- Utilities


(<|) :: (a -> b) -> a -> b
f <| x = f x
infixr 0 <|


(|>) :: a -> (a -> b) -> b
x |> f = f x
infixl 0 |>


identity :: a -> a
identity = id


-- Experimental

-- Record accessors instead of composition... we _can_ do it... but should we?
-- No idea what the state of the relevant proposals is...
-- https://ghc.haskell.org/trac/ghc/wiki/Records/DeclaredOverloadedRecordFields/DotPostfix
-- https://ghc.haskell.org/trac/ghc/wiki/Records/DeclaredOverloadedRecordFields
--
-- Cases against this:
-- >>> putStrLn $ app.view_ $ app.init_
-- You cannot remove the second $, otherwise Haskell reads it like so
-- >>> putStrLn $ app.view_ app.init_
-- >>> putStrLn $ app . view_ app . init_
-- >>> putStrLn $ app . (view_ app) . init_
-- And you get the confusingly obscure for begginers error:
--
-- • Couldn't match type ‘Program App.Model App.Msg’ with ‘Int’
--   Expected type: Program App.Model App.Msg -> String
--     Actual type: App.Model -> String
-- • In the second argument of ‘(.)’, namely ‘view_ app’
--   In the first argument of ‘(.)’, namely ‘app . view_ app’
--   In the second argument of ‘($)’, namely ‘app . view_ app . init_’
-- /Users/mario/dev/projects/oak/src/OakRuntime.hs: 28, 32
-- • Couldn't match type ‘[Char]’ with ‘Program String msg0’
--   Expected type: String -> String
--     Actual type: Program String msg0 -> String
-- • In the second argument of ‘(.)’, namely ‘init_’
--   In the second argument of ‘($)’, namely ‘app . view_ app . init_’
--   In a stmt of a 'do' block: putStrLn $ app . view_ app . init_
--
-- >>> let (newModel, cmd) = (app.update_) model msg
-- Even $ doesn't help us
-- >>> let (newModel, cmd) = app.update_ $ model msg
-- >>> let (newModel, cmd) = app . update_ $ model msg
-- >>> let (newModel, cmd) = app . (update_ $ model msg)
-- So effectively that leaves us with the abiguous bi-state
--
-- One argument... kinda ok
-- >>> record.attribute $ arg1
--
-- Two arguments... needs brackets
-- (record.attribute) arg1 arg2

(.) :: a -> (a -> b) -> b
(.) record f = f record


debug :: String -> String -> ()
debug x y = unsafePerformIO $ do
  putStrLn x
  putStrLn y


sleep :: Int -> IO ()
sleep milliseconds = threadDelay (milliseconds * 1000)
