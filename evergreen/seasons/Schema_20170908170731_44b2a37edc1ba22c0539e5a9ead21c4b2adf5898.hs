module Schema_20170908170731_44b2a37edc1ba22c0539e5a9ead21c4b2adf5898
       where
import HiltPostgres

data User = User{id :: Int, name :: String}
migration db = do removeColumn db "user" "surname"