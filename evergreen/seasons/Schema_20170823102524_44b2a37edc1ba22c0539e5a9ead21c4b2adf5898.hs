module Schema_20170823102524_44b2a37edc1ba22c0539e5a9ead21c4b2adf5898
       where

data User = User{id :: Int, name :: String}
migration db
  = do addColumn db "user" "id" "Int"
       removeColumn db "user" "surname"