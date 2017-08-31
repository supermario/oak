module Migrations where

import qualified Hilt
import qualified Hilt.Postgres
import HiltPostgres
import Data.Text (Text)
import Data.List as List

import qualified Schema_20170823221628_c1474a1f412a2b4bb8f7c24a4ab5b54398825258

import Data.Function ((&))
(|>) = (&)


allMigrations :: [(Text, Hilt.Postgres.Handle -> IO ())]
allMigrations =
  [ ("init", noMigration)
  , ( "c1474a1f412a2b4bb8f7c24a4ab5b54398825258"
    , Schema_20170823221628_c1474a1f412a2b4bb8f7c24a4ab5b54398825258.migration
    )
  ]

noMigration db = pure ()


migrationsFor :: Text -> Hilt.Postgres.Handle -> Either String [IO ()]
migrationsFor sha db = case sha of
  "init" -> Right $ fmap (\(s, m) -> m db) allMigrations
  _      -> seasonMigration sha db


migrationExists :: Text -> Bool
migrationExists dbSha = any ((==dbSha) . fst) allMigrations


seasonMigration :: Text -> Hilt.Postgres.Handle -> Either String [IO ()]
seasonMigration season db = case migrationsOnwardFrom season of
  Nothing         -> Left "Unknown season! No migrations run."
  Just []         -> Right [putStrLn "Migrations up to date."]
  Just migrations -> Right (fmap (\m -> m db) migrations)


migrationsOnwardFrom :: Text -> Maybe [Hilt.Postgres.Handle -> IO ()]
migrationsOnwardFrom season = do
  let indexM = List.findIndex ((==season) . fst) allMigrations
  case indexM of
    Nothing    -> Nothing
    Just index -> Just $ snd <$> List.drop (index + 1) allMigrations
