{-# LANGUAGE DuplicateRecordFields #-}
module Schema_20171105162631_3038a271e40837c57156402364c9118dfebd9573
       where
import HiltPostgres

data User = User{id :: Int, nameX :: String}

data Account = Account{id :: Int, userIdx :: Int}
migration db = do
  createTable db "account"
  addColumn db "account" "id"      "serial primary key"
  addColumn db "account" "userIdx" "int8 not null"
  removeColumn db "user" "name"
  addColumn db "user" "nameX" "varchar(255) not null"
