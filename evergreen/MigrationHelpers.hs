module MigrationHelpers where

import qualified Hilt.Postgres
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.List as List
import Data.Monoid ((<>))


noMigration :: Hilt.Postgres.Handle -> IO ()
noMigration db = pure ()


initPrefix :: [(Text, Hilt.Postgres.Handle -> IO ())] -> [(Text, Hilt.Postgres.Handle -> IO ())]
initPrefix migrations = ("init", noMigration) : migrations


migrationExists :: [(Text, Hilt.Postgres.Handle -> IO ())] -> Text -> Bool
migrationExists allMigrations dbSha = any ((==dbSha) . fst) (initPrefix allMigrations)


prepMigrations :: [(Text, Hilt.Postgres.Handle -> IO ())] -> Hilt.Postgres.Handle -> [IO ()]
prepMigrations migrations db = fmap announced migrations
 where
  announced (season, migration) = do
    if season /= "init" then T.putStrLn $ "Season " <> season else pure ()
    _ <- migration db
    pure ()


migrationsFor :: [(Text, Hilt.Postgres.Handle -> IO ())] -> Text -> Hilt.Postgres.Handle -> Either Text [IO ()]
migrationsFor allMigrations sha db = case sha of
  "init" -> Right $ prepMigrations prefixedMigrations db
  _      -> seasonMigration sha db
 where

  prefixedMigrations = initPrefix allMigrations

  seasonMigration :: Text -> Hilt.Postgres.Handle -> Either Text [IO ()]
  seasonMigration season db_ = case migrationsOnwardFrom season of
    Nothing         -> Left $ "Error: I was asked to find migrations onward from SHA " <> season <> " but none exist."
    Just []         -> Right [putStrLn "Migrations up to date."]
    Just migrations -> Right $ prepMigrations migrations db_


  migrationsOnwardFrom :: Text -> Maybe [(Text, Hilt.Postgres.Handle -> IO ())]
  migrationsOnwardFrom season = do
    let indexM = List.findIndex ((==season) . fst) prefixedMigrations
    case indexM of
      Nothing    -> Nothing
      Just index -> Just $ List.drop (index + 1) prefixedMigrations
