module App where

import Prelude hiding (init)
import Oak

-- main :: Program Types.Model Types.Msg
main :: Program
main =
  Program
    { init_ = 0
    , update_ = update
    , subscriptions_ = subscriptions
    , view_ = view
    }

view model = show model

update :: Model -> Msg -> Model
update model msg = case msg of
  Increment -> model + 1
  Decrement -> model - 1
  Noop -> model

subscriptions = []
