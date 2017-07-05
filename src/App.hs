{-# LANGUAGE RecordWildCards #-}

module App where

import Oak
import Data.Text (Text)

data Model = Model
  { counter :: Int
  , message :: Text
  } deriving Show


data Msg
  = Increment
  | Decrement
  | KeyPress String
  | SocketJoined SocketId Int
  | SocketReceive SocketId Text
  | Noop


main :: Program Model Msg
main = Program initial update view subscriptions


initial :: Model
initial = Model { counter = 0, message = "" }


update :: Model -> Msg -> (Model, Cmd Msg)
update model@Model{..} msg = case msg of
  Increment -> (model { counter = counter + 1 }, cmdNone)

  Decrement -> (model { counter = counter - 1 }, cmdNone)

  Noop -> (model, cmdNone)

  KeyPress key ->
    case key of
      "\n" -> (model, cmdNone)
      "u"  -> (model { counter = counter + 1 }, cmdNone)
      "d"  -> (model { counter = counter - 1 }, cmdNone)
      "b"  -> (model, socketBroadcast "Testing!")
      _    -> (model, cmdNone)

  SocketJoined _ _ -> (model { message = "User joined!" }, cmdNone)

  SocketReceive _ text -> (model { message = text }, cmdNone)


view :: Model -> String
view = show


subscriptions :: [Subscription Msg]
subscriptions =
  [ keypressListen KeyPress
  , websocketListen "/ws" SocketJoined SocketReceive ]
