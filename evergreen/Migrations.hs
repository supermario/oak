module Migrations where
import qualified Hilt.Postgres
import Data.Text (Text)
import qualified
       Schema_20170823221628_c1474a1f412a2b4bb8f7c24a4ab5b54398825258

allMigrations :: [(Text, Hilt.Postgres.Handle -> IO ())]
allMigrations
  = [("c1474a1f412a2b4bb8f7c24a4ab5b54398825258",
      Schema_20170823221628_c1474a1f412a2b4bb8f7c24a4ab5b54398825258.migration)]