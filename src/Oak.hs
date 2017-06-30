module Oak where

import Prelude hiding (init)
-- import Navigation
-- import Types
-- import State
-- import View

data Program = Program
  { init_ :: Int
  , update_ :: Model -> Msg -> Model
  , view_ :: Model -> String
  , subscriptions_ :: [Int]
  }

type Model = Int

data Msg = Increment | Decrement | Noop deriving (Read, Show, Eq)


(<|) :: (a -> b) -> a -> b
f <| x = f x
infixr 0 <|

(|>) :: a -> (a -> b) -> b
x |> f = f x
infixl 0 |>

identity = id
