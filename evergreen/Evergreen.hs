{-# LANGUAGE ScopedTypeVariables #-}


module Main where

import qualified Database.PostgreSQL.Simple     as SQL
import qualified Hilt
import qualified Hilt.Postgres
import Hilt.Handles.Postgres

import Language.Haskell.Exts.Simple
import Data.List ((\\), find)
import Data.Maybe (fromMaybe)

import Database.PostgreSQL.Migrations
import Database.PostgreSQL.Escape (quoteIdent)
import Database.PostgreSQL.Simple.Types (Query(..))
import qualified Data.ByteString.Char8 as S8

import qualified Data.Text as T

import Debug.Trace
import Data.Int
import Turtle
import qualified Control.Foldl as Fold
import Model


-- @TODO read from package.yaml using hpack lib
applicationName :: String
applicationName = "oak"


main :: IO ()
main = Hilt.manageOnce $ do

  -- @TODO check if the DB exists and be more helpful
  db <- Hilt.Postgres.load

  Hilt.program $ do

    -- createTable db "model"
    -- addColumn db "model" "name" "varchar(255) not null"
    -- addColumn db "model" "surname" "varchar(255) not null"
    -- removeColumn db "model" "surname"

    -- dbInfo <- Hilt.Postgres.dbInfo db
    -- Hilt.Postgres.pp dbInfo

    let currentModel = "ModelA" -- @TODO how will we do this dynamically?

    modelAst <- loadModelAst currentModel

    -- case dbStatus dbInfo modelAst of
    --   Clean   -> putStrLn "DB is up to date with code"
    --   Dirty   -> do
    --     let
    --       dbModelAst = moduleDataDecl $ dbInfoToAst dbInfo
    --       codeModelAst = moduleDataDecl modelAst
    --
    --     print $ diff dbModelAst codeModelAst
    --
    --     putStrLn "Model changed since last blah..."
    --
    --   Pending -> putStrLn "Pending migrations"

    let filename = "evergreen/seasons/20170721145900_Schema_A.hs"

    s <- evergreenStatus "evergreen/Schema.hs"
    case s of
      Uninitiated -> do
        putStrLn $ "Writing first season to " ++ filename
        writeSeasonAst filename $ astModel "Model" []

      Uncommitted -> do
        writeSeasonAst filename $ astModel "Model_A" []

        putStrLn "Schema changes to be committed:"
        putStrLn $ "  (season remembered in "++filename++")"
        putStrLn ""
        putStrLn "  New record: User"
        putStrLn ""
        putStrLn "        added:    User.name        :: String"
        putStrLn "        added:    User.firstname   :: String"
        putStrLn "        added:    User.age         :: Int"
        putStrLn "        added:    User.dateOfBirth :: UTCTime"
        putStrLn ""

      Committed ->
        putStrLn "Nothing to commit, schema is in season\n"

      Deleted -> do
        putStrLn "It looks like evergreen/Schema.hs has been deleted, which seems really bad..."
        putStrLn "Perhaps you want to `git checkout evergreen/Schema.hs` to restore it?"

      UnexpectedEvergreenStatus ->
        putStrLn "Got an unexpected Evergreen status... please check `evergreenStatus`"


lastSeasonFilename :: IO (Maybe Turtle.FilePath)
lastSeasonFilename =
  fold (Turtle.find (suffix ".hs") "evergreen/seasons") Fold.last


evergreenStatus :: Text -> IO EvergreenStatus
evergreenStatus schema = do
  gitStatus <- strict $ inshell ("git status --porcelain " <> schema) empty
  print $ firstTwo gitStatus
  case firstTwo gitStatus of
    ('_','_')       -> do
      -- `git status` is empty, so we need to check if the file is tracked (thus clean) or non-existent
      gitFiles <- strict $ inshell ("git ls-files " <> schema) empty
      case gitFiles of
        "" -> return Uninitiated
        _ -> return Committed

    ('?', _)   -> return Uncommitted -- Untracked, but exists
    ('A', _)   -> return Uncommitted -- Added, so not yet committed
    (' ', 'M') -> return Uncommitted -- Modified
    ('M', ' ') -> return Staged      -- Added, so not yet committed

    (' ', 'D') -> return Deleted -- Added, so not yet committed


    -- @TODO other statuses other than Uninitiatied
    -- https://git-scm.com/docs/git-status#_short_format
    _ ->  return UnexpectedEvergreenStatus


firstTwo :: Text -> (Char, Char)
firstTwo text = (first, second)
  where
    part = snd <$> T.uncons text
    first = fromMaybe '_' $ fst <$> T.uncons text
    second = fromMaybe '_' $ fst <$> (T.uncons =<< part)


data EvergreenStatus
  = Uninitiated
  | Uncommitted
  | Committed
  | Staged
  | Deleted
  | UnexpectedEvergreenStatus
  deriving (Show)


dbStatus :: Hilt.Postgres.DbInfo -> Module -> DbStatus
dbStatus dbInfo ast = do

  let dbAst    = dbInfoToAst dbInfo
      modelAst = ast

  if dbAst == modelAst then Clean else Dirty


dbInfoToAst :: Hilt.Postgres.DbInfo -> Module -- @TODO Maybe Module
dbInfoToAst dbInfo = do
  let
    target = "model" -- @HARDCODED paramaterise this later
    predicate TableInfo{..} = tableName == target

  case Data.List.find predicate dbInfo of
    Just TableInfo{..} ->
      astModel "Model" (fmap fieldInfoToField fields)

    Nothing -> astModel "Model" []

  -- @TODO cover if database is empty case


fieldInfoToField :: FieldInfo -> Field
fieldInfoToField FieldInfo{..} = (fieldName, tipe, fieldNullable)

  where tipe = case fieldType of
          "character varying(255)" -> "String"
          -- @TODO handle various postgres types
          -- @TODO go steal the definitions from Persistent
          -- https://github.com/yesodweb/persistent/wiki/Persistent-entity-syntax


data DbStatus
  = Clean
  | Pending
  | Dirty
  deriving (Show)


-- @TODO make this typesafe
-- (name, type)
type Field = (String, String, Bool)


loadModelAst :: String -> IO Module
loadModelAst modelVersion = fromParseResult <$> parseFile ("evergreen/" ++ modelVersion ++ ".hs")


writeSeasonAst :: String -> Module -> IO ()
writeSeasonAst filename ast = writeFile filename $ prettyPrint ast


astModel :: String -> [Field] -> Module
astModel name fields = do
  let
    fieldDecls = fmap (\(name, tipe, _) ->
        -- @TODO maybe on bool?
        FieldDecl [Ident name] (TyCon (UnQual (Ident tipe)))
      ) fields


  Module (Just (ModuleHead (ModuleName name) Nothing Nothing)) [] []
    [
      DataDecl
        DataType Nothing (DHead (Ident name))
        [QualConDecl Nothing Nothing (RecDecl (Ident name) fieldDecls)]
        Nothing
    ]


data Diff
  = Added Field
  | Removed Field
  | Debug String
  deriving (Show)


diff :: Decl -> Decl -> [Diff]
diff d1 d2 =
  if areDataDecls d1 d2 then do
    let
      f1 = fieldDecs d1
      f2 = fieldDecs d2
      added = Added <$> (f2 \\ f1)
      removed = Removed <$> (f1 \\ f2)

    added ++ removed

  else
    [Debug "Cannot compare structures that aren't data declarations"]


moduleDataDecl :: Module -> Decl
moduleDataDecl (Module (Just (ModuleHead (ModuleName name) Nothing Nothing)) [] [] [dataDecl]) = dataDecl
moduleDataDecl _ = undefined


fieldDecs :: Decl -> [Field]
fieldDecs (DataDecl _ _ _ (QualConDecl Nothing Nothing (RecDecl (Ident _) fieldDecls):xs) _)
  = fmap fieldDecltoField fieldDecls
fieldDecs d = [("Error:fieldDecs", "Field is not a DataDecl with QualConDecl", False)]


fieldDecltoField :: FieldDecl -> Field
-- Need to transform the Maybe here
fieldDecltoField (FieldDecl (Ident name:xs) (TyCon (UnQual (Ident tipe)))) = (name, tipe, False)
fieldDecltoField _ = ("Error:fieldDecltoField", "Field doens't match shape", False)


areDataDecls :: Decl -> Decl -> Bool
areDataDecls DataDecl{} DataDecl{} = True
areDataDecls _        _            = False



createTable :: Hilt.Handles.Postgres.Handle -> String -> IO ()
createTable db table = do
  Hilt.Postgres.execute db (tryCreateTableStmt (S8.pack table)) ()
  return ()


tryCreateTableStmt :: S8.ByteString
                  -- ^ Table name
                  -> SQL.Query
tryCreateTableStmt tableName =
  Query $ S8.concat [ "create table if not exists ", quoteIdent tableName, " ();"]


addColumn :: Hilt.Handles.Postgres.Handle -> String -> String -> String -> IO ()
addColumn db table column tipe = do
  Hilt.Postgres.execute db (add_column_stmt (S8.pack table) (S8.pack column) (S8.pack tipe)) ()
  return ()


removeColumn :: Hilt.Handles.Postgres.Handle -> String -> String -> IO ()
removeColumn db table column = do
  Hilt.Postgres.execute db (drop_column_stmt (S8.pack table) (S8.pack column)) ()
  return ()




-- TESTS

testSample :: IO ()
testSample = prettyPrint . fromParseResult <$> parseFile "evergreen/ModelA.hs" >>= putStrLn


testAst :: IO ()
testAst = putStrLn $ prettyPrint $ astModel "ModelA" []

--- Mocks

{-

name        varchar(255) NOT NULL
title       varchar(40) NOT NULL
did         integer NOT NULL,
date_prod   date,
kind        varchar(10),
len         interval hour to minute

-}
