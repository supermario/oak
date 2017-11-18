module Evergreen where

import qualified Hilt.Postgres

-- https://github.com/alevy/postgresql-orm
import qualified Database.PostgreSQL.Simple as SQL
import Database.PostgreSQL.Migrations (add_column_stmt, drop_column_stmt)
import Database.PostgreSQL.Escape (quoteIdent)
import Database.PostgreSQL.Simple.Types (Query(..))
import qualified Data.ByteString.Char8 as S8
import Data.Time (UTCTime)
import Data.Text as T
import qualified Data.Text.IO as T

import Data.Monoid ((<>))

import MigrationHelpers (migrationExists, migrationsFor)
import ShellHelpers (tShow, sha1)
import AstDatabase (dbInfoToAst, showDbDiff)

type PrimaryInt = Int
type Datetime = UTCTime

data Migration
  = CreateTable String
  | DropTable String
  | AddColumn String String String
  | RemoveColumn String String
  deriving (Show)


-- @TODO currently all these helpers execute IO directly – ideally we'd have a
-- set of statements return in a migration scenario so we can run them all in a
-- single transaction if possible


migrate :: Hilt.Postgres.Handle -> [(Text, Hilt.Postgres.Handle -> IO ())] -> IO ()
migrate db allMigrations = do
  -- @TODO Temporary while we're testing
  dropAllTables db

  dbInfo <- Hilt.Postgres.dbInfo db
  Hilt.Postgres.pp dbInfo

  case dbInfo of
    [] -> do
      putStrLn "Database is empty, initialising."
      displayOrRun "init" db allMigrations

    _ -> do
      -- Look at the DB schema and generate a SHA that identifies its AST
      let dbSha = dbShaText dbInfo

      -- Check if this SHA is a known season amongst our current migrations
      if migrationExists allMigrations dbSha
        -- Ok, lets go ahead and try to process the migration from the current SHA onwards
        then displayOrRun dbSha db allMigrations
        else do
          -- @TODO How can we make these messages more user friendly?
          T.putStrLn $ "Error: database current state of " <> dbSha <> " does not match any known seasons."
          T.putStrLn "It's not safe to proceed, so I'm bailing out!"


-- Something like
-- Hilt.evergreen $ do
-- Could do the magic to handle this under the hood?
-- Or perhaps even better, an Evergreen version of the postgres service, that does this on load?
-- db <- Hilt.Evergreen.load


dbShaText :: Hilt.Postgres.DbInfo -> Text
dbShaText dbInfo = tShow $ sha1 $ tShow $ dbInfoToAst dbInfo


displayOrRun :: Text -> Hilt.Postgres.Handle -> [(Text, Hilt.Postgres.Handle -> IO ())] -> IO ()
displayOrRun dbSha db allMigrations = case migrationsFor allMigrations dbSha db of
  Left err_ -> do
    T.putStrLn err_
    showDbDiff db

  Right migrations -> do
    T.putStrLn $ "Fetching migrations for DB SHA: " <> dbSha
    sequence_ migrations
    pure ()


-- Re-export this type so migration files compile strictly for now, until we address the above point and remove db dep
type PostgresHandle = Hilt.Postgres.Handle


migrationToIO :: Migration -> Hilt.Postgres.Handle -> IO ()
migrationToIO migration db = case migration of
  CreateTable tableName                   -> createTable db tableName
  DropTable   tableName                   -> dropTable db tableName
  AddColumn tableName fieldName fieldType -> addColumn db tableName fieldName fieldType
  RemoveColumn tableName fieldName        -> removeColumn db tableName fieldName


createTable :: Hilt.Postgres.Handle -> String -> IO ()
createTable db table = do
  putStrLn $ "db: creating " <> table <> " table"
  _ <- Hilt.Postgres.execute db (tryCreateTableStmt (S8.pack table)) ()
  pure ()


-- Do we really want a try? Would we rather things failed?
tryCreateTableStmt :: S8.ByteString -> SQL.Query
tryCreateTableStmt tableName = Query $ S8.concat ["create table if not exists ", quoteIdent tableName, " ();"]


addColumn :: Hilt.Postgres.Handle -> String -> String -> String -> IO ()
addColumn db table column tipe = do
  putStrLn $ "db: adding " <> table <> "." <> column <> " (" <> tipe <> ")"
  _ <- Hilt.Postgres.execute db (add_column_stmt (S8.pack table) (S8.pack column) (S8.pack tipe)) ()
  pure ()


removeColumn :: Hilt.Postgres.Handle -> String -> String -> IO ()
removeColumn db table column = do
  putStrLn $ "db: removing " <> table <> "." <> column
  _ <- Hilt.Postgres.execute db (drop_column_stmt (S8.pack table) (S8.pack column)) ()
  pure ()


dropTable :: Hilt.Postgres.Handle -> String -> IO ()
dropTable db table = do
  putStrLn $ "db: dropping " <> table
  _ <- Hilt.Postgres.execute db (tryDropTableStmt (S8.pack table)) ()
  pure ()


-- Do we really want a try? Would we rather things failed?
tryDropTableStmt :: S8.ByteString -> SQL.Query
tryDropTableStmt tableName = Query $ S8.concat ["drop table if exists ", quoteIdent tableName, ";"]



dropAllTables :: Hilt.Postgres.Handle -> IO ()
dropAllTables db = do
  putStrLn "db: dropping all tables"
  _ <- Hilt.Postgres.execute db tryDropAllTablesStmt ()
  pure ()


tryDropAllTablesStmt :: SQL.Query
tryDropAllTablesStmt = Query $ S8.concat
  [ "DROP SCHEMA public CASCADE;"
  , "CREATE SCHEMA public;"
  , "GRANT ALL ON SCHEMA public TO postgres;"
  , "GRANT ALL ON SCHEMA public TO public;"
  ]
