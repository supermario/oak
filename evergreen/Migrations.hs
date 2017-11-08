module Migrations where
import qualified Hilt.Postgres
import Data.Text (Text)

allMigrations :: [(Text, Hilt.Postgres.Handle -> IO ())]
allMigrations = []
