module Migrations where
import qualified Hilt.Postgres
import Data.Text (Text)
import qualified
       Schema_20171117164341_53a4707384760633c4abb61951c4d34a835d05be

allMigrations :: [(Text, Hilt.Postgres.Handle -> IO ())]
allMigrations
  = [("53a4707384760633c4abb61951c4d34a835d05be",
      Schema_20171117164341_53a4707384760633c4abb61951c4d34a835d05be.migration)]