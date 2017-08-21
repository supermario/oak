module Migrations where

import qualified Hilt
import qualified Hilt.Postgres
import HiltPostgres
import Data.Text (Text)
import Data.List as List

import Data.Function ((&))
(|>) = (&)


data Season
  = TheBeggining
  | UnknownSeason
  | Schema_20170812141450_be119f5af8585265f8a03acda4d86dfe6eaecb22
  deriving (Eq)

seasonFromSha :: Text -> Season
seasonFromSha sha = case sha of
  "be119f5af8585265f8a03acda4d86dfe6eaecb22" ->
    Schema_20170812141450_be119f5af8585265f8a03acda4d86dfe6eaecb22
  "init" -> TheBeggining
  _ -> UnknownSeason


migrationsFor :: Text -> Hilt.Postgres.Handle -> Either String [IO ()]
migrationsFor x = seasonMigration (seasonFromSha x)


seasonMigration :: Season -> Hilt.Postgres.Handle -> Either String [IO ()]
seasonMigration season db =
  case season of
    UnknownSeason ->
      Left "Unknown season! No migrations run."

    _ -> do
      let migrations = migrationsOnwardFrom season db

      case migrations of
        [] -> Right [putStrLn "Migrations up to date."]
        _ -> Right migrations


migrationsOnwardFrom season db = do
  let indexM = List.findIndex ((== season) . fst) (orderedMigrations db)
  case indexM of
    Nothing -> []
    Just index -> snd <$> List.drop (index + 1) (orderedMigrations db)


orderedMigrations db =
  [ (TheBeggining, pure ())
  , (Schema_20170812141450_be119f5af8585265f8a03acda4d86dfe6eaecb22, do
      createTable db "user"
      addColumn db "user" "name" "varchar(255) not null"
      addColumn db "user" "surname" "varchar(255) not null"
    )
  ]
