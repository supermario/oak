module Migrations where
import qualified Hilt.Postgres
import Data.Text (Text)
import qualified
       Seasons.Schema_20171118004219_53a4707384760633c4abb61951c4d34a835d05be

allMigrations :: [(Text, Hilt.Postgres.Handle -> IO ())]
allMigrations
  = [("53a4707384760633c4abb61951c4d34a835d05be",
      Seasons.Schema_20171118004219_53a4707384760633c4abb61951c4d34a835d05be.migration)]