{-# LANGUAGE DuplicateRecordFields #-}
module Schema_20171105173923_d7cabc46feefe09136f396fc01c558f7a5b4c31a
       where
import HiltPostgres

data User = User{id :: Int, nameX :: String}

data Account = Account{id :: Int, userIdx :: Maybe Int}
migration db
  = do removeColumn db "account" "userIdx"
       addColumn db "account" "userIdx" "int8"