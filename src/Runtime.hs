-- Trying out the Elm architecture with Hilt

module Main where

import Prelude hiding (init)
-- import Navigation
-- import Types
-- import State
-- import View



import qualified App
import qualified Oak

main :: IO ()
main = do
  putStrLn "Starting..."
  putStrLn $ Oak.view_ app model
  putStrLn $ Oak.view_ app newModel

  where app = App.main
        model = Oak.init_ app
        newModel = App.update model Oak.Increment
