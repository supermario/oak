module HiltPostgres where

import qualified Hilt
import qualified Hilt.Postgres

import qualified Database.PostgreSQL.Simple as SQL
import Database.PostgreSQL.Migrations
import Database.PostgreSQL.Escape (quoteIdent)
import Database.PostgreSQL.Simple.Types (Query(..))
import qualified Data.ByteString.Char8 as S8


createTable :: Hilt.Postgres.Handle -> String -> IO ()
createTable db table = do
  Hilt.Postgres.execute db (tryCreateTableStmt (S8.pack table)) ()
  pure ()


tryCreateTableStmt :: S8.ByteString
                  -- ^ Table name
                  -> SQL.Query
tryCreateTableStmt tableName =
  Query $ S8.concat [ "create table if not exists ", quoteIdent tableName, " ();"]


addColumn :: Hilt.Postgres.Handle -> String -> String -> String -> IO ()
addColumn db table column tipe = do
  Hilt.Postgres.execute db (add_column_stmt (S8.pack table) (S8.pack column) (S8.pack tipe)) ()
  pure ()


removeColumn :: Hilt.Postgres.Handle -> String -> String -> IO ()
removeColumn db table column = do
  Hilt.Postgres.execute db (drop_column_stmt (S8.pack table) (S8.pack column)) ()
  pure ()
