{-# LANGUAGE DuplicateRecordFields #-}
module Schema_20171103091331_8fa6cea725448309c8292f8179b5bc456c08d5bb
       where
import HiltPostgres

data User = User{id :: Int, name :: String}

data Account = Account{id :: Int, userId :: Int}
migration db = pure ()
