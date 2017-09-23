module Migrations where
import qualified Hilt.Postgres
import Data.Text (Text)
import qualified
       Schema_20170823221628_c1474a1f412a2b4bb8f7c24a4ab5b54398825258
import qualified
       Schema_20170908170731_44b2a37edc1ba22c0539e5a9ead21c4b2adf5898

allMigrations :: [(Text, Hilt.Postgres.Handle -> IO ())]
allMigrations
  = [("c1474a1f412a2b4bb8f7c24a4ab5b54398825258",
      Schema_20170823221628_c1474a1f412a2b4bb8f7c24a4ab5b54398825258.migration),
     ("44b2a37edc1ba22c0539e5a9ead21c4b2adf5898",
      Schema_20170908170731_44b2a37edc1ba22c0539e5a9ead21c4b2adf5898.migration)]