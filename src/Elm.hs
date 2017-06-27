-- Trying out the Elm architecture with Hilt

module Main where

import Prelude hiding (init)
-- import Navigation
-- import Types
-- import State
-- import View



import qualified App
import qualified AppTypes as App

main :: IO ()
main = do
  putStrLn "Starting..."
  putStrLn $ App.view_ app model
  putStrLn $ App.view_ app newModel

  where app = App.main
        model = App.init_ app
        newModel = App.update model App.Increment
