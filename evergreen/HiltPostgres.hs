module HiltPostgres where

import qualified Hilt
import qualified Hilt.Postgres

import qualified Database.PostgreSQL.Simple as SQL
import Database.PostgreSQL.Migrations
import Database.PostgreSQL.Escape (quoteIdent)
import Database.PostgreSQL.Simple.Types (Query(..))
import qualified Data.ByteString.Char8 as S8

import Data.Monoid ((<>))

-- @TODO currently all these helpers execute IO directly – ideally we'd have a
-- set of statements return in a migration scenario so we can run them all in a
-- single transaction if possible

createTable :: Hilt.Postgres.Handle -> String -> IO ()
createTable db table = do
  putStrLn $ "db: creating " <> table <> " table"
  Hilt.Postgres.execute db (tryCreateTableStmt (S8.pack table)) ()
  pure ()


tryCreateTableStmt :: S8.ByteString
                  -- ^ Table name
                  -> SQL.Query
tryCreateTableStmt tableName =
  Query $ S8.concat [ "create table if not exists ", quoteIdent tableName, " ();"]


addColumn :: Hilt.Postgres.Handle -> String -> String -> String -> IO ()
addColumn db table column tipe = do
  putStrLn $ "db: adding " <> table <> "." <> column <> " (" <> tipe <> ")"
  Hilt.Postgres.execute db (add_column_stmt (S8.pack table) (S8.pack column) (S8.pack tipe)) ()
  pure ()


removeColumn :: Hilt.Postgres.Handle -> String -> String -> IO ()
removeColumn db table column = do
  putStrLn $ "db: removing " <> table <> "." <> column
  Hilt.Postgres.execute db (drop_column_stmt (S8.pack table) (S8.pack column)) ()
  pure ()


dropTable :: Hilt.Postgres.Handle -> String -> IO ()
dropTable db table = do
  putStrLn $ "db: dropping " <> table
  Hilt.Postgres.execute db (drop_table_stmt (S8.pack table)) ()
  pure ()
