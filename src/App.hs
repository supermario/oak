module App where

import Prelude hiding (init)
import Oak

-- main :: Program Types.Model Types.Msg
main :: Program
main = Program 0 update view subscriptions

view = show

update :: Model -> Msg -> Model
update model msg = case msg of
  Increment -> model + 1
  Decrement -> model - 1
  Noop -> model
  KeyPress key ->
    case key of
      "\n" -> model
      "u"  -> model + 1
      "d"  -> model - 1
      _    -> model

subscriptions = [keySubscription KeyPress]
