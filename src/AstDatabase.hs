module AstDatabase where


import qualified Hilt.Postgres

import Language.Haskell.Exts.Simple
import Data.List (find)
import AstSchema (loadSchemaAst)
import AstHelpers (astModel, Field, moduleDataDecls, diff)


data DbStatus
  = Clean
  | Pending
  | Dirty
  deriving (Show)


dbStatus :: Hilt.Postgres.DbInfo -> Module -> DbStatus
dbStatus dbInfo ast = do

  let dbAst    = dbInfoToAst dbInfo
      modelAst = ast

  if dbAst == modelAst then Clean else Dirty


dbInfoToAst :: Hilt.Postgres.DbInfo -> Module -- @TODO Maybe Module
dbInfoToAst dbInfo = do
  let target = "user" -- @HARDCODED paramaterise this later
      predicate Hilt.Postgres.TableInfo {..} = tableName == target

  case Data.List.find predicate dbInfo of
    Just Hilt.Postgres.TableInfo {..} -> astModel "Schema" "User" (fmap fieldInfoToField fields)

    Nothing                           -> astModel "Schema" "X" []

  -- @TODO cover if database is empty case


fieldInfoToField :: Hilt.Postgres.FieldInfo -> Field
fieldInfoToField Hilt.Postgres.FieldInfo {..} = (fieldName, tipe, fieldNullable)
 where
  tipe = case fieldType of
    "character varying(255)" ->
      "String"
          -- @TODO handle various postgres types
          -- @TODO go steal the definitions from Persistent
          -- https://github.com/yesodweb/persistent/wiki/Persistent-entity-syntax

    "integer" ->
      "Int"

          -- @TODO ERROR: have a wildcard to catch exceptions...? Or throw exception?
    _ -> "UnknownField"


showDbDiff :: Hilt.Postgres.Handle -> IO ()
showDbDiff db = do
  schemaAst <- loadSchemaAst "Schema"
  dbInfo    <- Hilt.Postgres.dbInfo db

  print schemaAst
  print $ dbInfoToAst dbInfo

  let dbModelAst   = moduleDataDecls $ dbInfoToAst dbInfo
      codeModelAst = moduleDataDecls schemaAst

  putStrLn "The differences between the DB and the latest Schema are:"
  print $ diff dbModelAst codeModelAst
