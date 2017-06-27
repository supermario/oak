-- Trying out the Elm architecture with Hilt

module AppTypes where

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

data Msg = Increment | Decrement | Noop
