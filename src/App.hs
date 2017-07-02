module App where

import Oak

type Model = Int

data Msg
  = Increment
  | Decrement
  | KeyPress String
  | Noop

main :: Program Model Msg
main = Program 0 update view subscriptions

update :: Model -> Msg -> (Model, Cmd Msg)
update model msg = case msg of
  Increment -> (model + 1, cmdNone)
  Decrement -> (model - 1, cmdNone)
  Noop -> (model, cmdNone)
  KeyPress key ->
    case key of
      "\n" -> (model, delay 1000 $ asTask Increment )
      "u"  -> (model + 1, cmdNone)
      "d"  -> (model - 1, cmdNone)
      "k"  -> (model, naughty)
      "e"  -> (model, exit)
      _    -> (model, cmdNone)

view = show

subscriptions = [keySubscription KeyPress]
