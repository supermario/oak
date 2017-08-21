module Schema_20170821102603_be119f5af8585265f8a03acda4d86dfe6eaecb22 where

import HiltPostgres


data User = User{name :: String, surnamex :: String}


migration db = do
  createTable db "user"
  addColumn db "user" "name" "varchar(255) not null"
  addColumn db "user" "surnamex" "varchar(255) not null"
