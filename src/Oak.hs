module Oak where

import Prelude hiding (init, (.))
import Control.Concurrent (threadDelay)

data Program model msg = Program
  { init_ :: model
  , update_ :: model -> msg -> (model, Cmd msg)
  , view_ :: model -> String
  , subscriptions_ :: [IO msg]
  }

data Cmd msg = Cmd { task_ :: IO (Maybe msg) }


cmdNone
  = Cmd { task_ = return Nothing }


task x
  = Cmd { task_ = x }


delay :: Int -> Cmd msg -> Cmd msg
delay milliseconds cmd =
  task $ do
    threadDelay (milliseconds * 1000)
    task_ cmd


naughty :: Cmd msg
naughty =
  task $ error "Oops! Error!"


asTask :: msg -> Cmd msg
asTask msg =
  task $ return $ Just msg

-- Subscriptions

keySubscription :: (String -> msg) -> IO msg
keySubscription msg = do
  x <- getChar
  return $ msg [x]

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
