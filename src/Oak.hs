module Oak where

import Prelude hiding (init)

data Program = Program
  { init_ :: Int
  , update_ :: Model -> Msg -> Model
  , view_ :: Model -> String
  , subscriptions_ :: [IO Msg]
  }

-- @TODO paramaterize runtime and move types to App
type Model = Int
data Msg
  = Increment
  | Decrement
  | KeyPress String
  | Noop

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

identity = id
