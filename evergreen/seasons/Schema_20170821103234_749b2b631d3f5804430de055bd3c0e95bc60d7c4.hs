module Schema_20170821103234_749b2b631d3f5804430de055bd3c0e95bc60d7c4 where

import HiltPostgres


data User = User{name :: String, surname :: String}


migration db = do
  removeColumn db "user" "surnamex"
  addColumn db "user" "surname" "varchar(255) not null"
