module Migrations where

import qualified Hilt
import qualified Hilt.Postgres
import HiltPostgres
import Data.Text (Text)
import Data.List as List

import qualified Schema_20170823221628_c1474a1f412a2b4bb8f7c24a4ab5b54398825258

import MigrationHelpers (noMigration)

allMigrations :: [(Text, Hilt.Postgres.Handle -> IO ())]
allMigrations =
  [ ("init", noMigration)
  , ( "c1474a1f412a2b4bb8f7c24a4ab5b54398825258"
    , Schema_20170823221628_c1474a1f412a2b4bb8f7c24a4ab5b54398825258.migration
    )
  ]
