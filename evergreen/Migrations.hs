module Migrations where

import qualified Hilt
import qualified Hilt.Postgres
import HiltPostgres
import Data.Text (Text)
import Data.List as List

import qualified Schema_20170821102603_be119f5af8585265f8a03acda4d86dfe6eaecb22
import qualified Schema_20170821103234_749b2b631d3f5804430de055bd3c0e95bc60d7c4

import Data.Function ((&))
(|>) = (&)


data Season
  = TheBeggining
  | UnknownSeason
  | Schema_20170821102603_be119f5af8585265f8a03acda4d86dfe6eaecb22
  | Schema_20170821103234_749b2b631d3f5804430de055bd3c0e95bc60d7c4
  deriving (Eq)


seasonFromSha :: Text -> Season
seasonFromSha sha = case sha of
  "be119f5af8585265f8a03acda4d86dfe6eaecb22" -> Schema_20170821102603_be119f5af8585265f8a03acda4d86dfe6eaecb22
  "749b2b631d3f5804430de055bd3c0e95bc60d7c4" -> Schema_20170821103234_749b2b631d3f5804430de055bd3c0e95bc60d7c4
  "init" -> TheBeggining
  _ -> UnknownSeason


orderedMigrations :: Hilt.Postgres.Handle -> [(Season, IO ())]
orderedMigrations db =
  [ (TheBeggining, pure ())
  , ( Schema_20170821102603_be119f5af8585265f8a03acda4d86dfe6eaecb22, Schema_20170821102603_be119f5af8585265f8a03acda4d86dfe6eaecb22.migration db)
  , ( Schema_20170821103234_749b2b631d3f5804430de055bd3c0e95bc60d7c4, Schema_20170821103234_749b2b631d3f5804430de055bd3c0e95bc60d7c4.migration db)
  ]


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
