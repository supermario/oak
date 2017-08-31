module Schema_20170823221628_c1474a1f412a2b4bb8f7c24a4ab5b54398825258
       where
import HiltPostgres

data User = User{id :: Int, name :: String, surname :: String}
migration db = do
  createTable db "user"
  addColumn db "user" "id"      "serial primary key"
  addColumn db "user" "name"    "varchar(255) not null"
  addColumn db "user" "surname" "varchar(255) not null"
