{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Database.PostgreSQL.Simple as SQL
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

import Data.Time (getCurrentTime, defaultTimeLocale, formatTime)

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Data.Int
import Turtle
import Filesystem.Path.CurrentOS (fromText)
import qualified Control.Foldl as Fold
import Safe

import Data.Text.Encoding as E
import Crypto.Hash


-- @TODO read from package.yaml using hpack lib
applicationName :: String
applicationName = "oak"


main :: IO ()
main = Hilt.manageOnce $ do

  -- @TODO check if the DB exists and be more helpful
  db <- Hilt.Postgres.load

  Hilt.program $ do

    let currentModel = "Schema" -- @TODO how will we do this dynamically?

    schemaAst <- loadSchemaAst currentModel

    let schemaSha = tShow $ sha1 $ tShow schemaAst

    -- createTable db "model"
    -- addColumn db "model" "name" "varchar(255) not null"
    -- addColumn db "model" "surname" "varchar(255) not null"
    -- removeColumn db "model" "surname"

    -- dbInfo <- Hilt.Postgres.dbInfo db
    -- Hilt.Postgres.pp dbInfo

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

    newSeasonFile <- newSeasonFile schemaSha

    schemaStatus <- gitStatus $ fromText "types/Schema.hs"

    -- There may or may not already be a season to compare to.
    (seasonFile, seasonStatus) <- checkExistingSeason

    case schemaStatus of
      ChangesPending ->
        -- Our Schema.hs has changes to be committed.

        case seasonStatus of
          Uninitiated -> do
            -- There are no prior seasons yet, create our first one
            T.putStrLn $ "Writing first season to " <> asText newSeasonFile
            writeSeasonAst newSeasonFile schemaAst
            showSeasonChanges newSeasonFile schemaAst

          ChangesPending -> do
            -- A season with pending changes exists (unfinished season) so update it
            T.putStrLn $ "Updating season " <> asText seasonFile

            -- Always remove the currently uncomitted season file to keep as close as possible
            -- to the commit timestamp, as well as always have the correct AST SHA
            rm seasonFile
            writeSeasonAst newSeasonFile schemaAst
            showSeasonChanges newSeasonFile schemaAst

          Committed -> do
            seasonSha <- fileAstSha seasonFile

            if schemaSha == seasonSha then
              T.putStrLn "Schema has not been commited, but already matches a known season. This is bad... you forgot to commit your Schema!"
            else do
              T.putStrLn $ "Writing next season to " <> asText newSeasonFile
              writeSeasonAst newSeasonFile schemaAst
              showSeasonChanges newSeasonFile schemaAst

          x -> do
            -- This shows the changes between last season, and the current Schema
            -- Changes are written to a new, uncommited season file
            -- showSeasonChanges
            T.putStrLn "What next?"
            print x
          -- showFirstSeason

      Uninitiated -> putStrLn "There is no Schema.hs!"
        -- No Schema.hs file exists... should we write a new one?

      Committed ->
        -- @TODO even though it's in season, it might not match. Need to diff SHA and test integrity?
        -- @TODO there are no known seasons and this says it's all good if we're committed
        putStrLn "Nothing to commit, schema is in season\n"

      Deleted -> do
        putStrLn "It looks like types/Schema.hs has been deleted, which seems really bad..."
        putStrLn "Perhaps you want to `git checkout types/Schema.hs` to restore it?"

      UnexpectedEvergreenStatus ->
        putStrLn "Got an unexpected Evergreen status... please check `evergreenStatus`"


showSeasonChanges :: Turtle.FilePath -> Module -> IO ()
showSeasonChanges seasonFile schemaAst = do
  lastKnownM <- findLastKnownSeason

  case lastKnownM of
    Nothing -> do
      let empty = moduleDataDecl $ astModel "Model" []

      showSeasonDiff seasonFile $ diff empty (moduleDataDecl schemaAst)

    Just lastKnownSeason ->
      if lastKnownSeason == seasonFile then
        T.putStrLn "The last known season is the same as the new season, which seems impossible. This seems like it must be a bug."
      else do
        lastKnownAst <- loadFileAst lastKnownSeason

        showSeasonDiff seasonFile $ diff (moduleDataDecl lastKnownAst) (moduleDataDecl schemaAst)


showSeasonDiff :: Turtle.FilePath -> [Diff] -> IO ()
showSeasonDiff seasonFile changes = do
  T.putStrLn ""
  T.putStrLn ""
  T.putStrLn ""
  T.putStrLn "Schema changes to be committed:"
  T.putStrLn $ "  (season remembered in " <> asText seasonFile <> ")"
  T.putStrLn ""
  mapM_ (putStrLn . toSeasonDiffText) changes
  T.putStrLn ""


toSeasonDiffText :: Diff -> String
toSeasonDiffText change =
  case change of
    -- @TODO respect bool
    Added   (name, tipe, bool) -> "        added:   " <> name <> " :: " <> tipe
    Removed (name, tipe, bool) -> "        removed: " <> name <> " :: " <> tipe


newSeasonFile :: Text -> IO Turtle.FilePath
newSeasonFile sha = do
  currentTime <- getCurrentTime

  let timestamp = formatTime defaultTimeLocale "%Y%m%d%H%M%S" currentTime

  -- @TODO suffix needs to be some sort of dynamic
  pure $ fromText $ "evergreen/seasons/" <> T.pack timestamp <> "_Schema_" <> sha <> ".hs"


seasonFiles :: IO [Turtle.FilePath]
seasonFiles = fold (Turtle.find (suffix ".hs") "evergreen/seasons") Fold.revList


findLastKnownSeason :: IO (Maybe Turtle.FilePath)
findLastKnownSeason = do
  seasons <- seasonFiles

  let toTuple file = do
        status <- gitStatus file
        pure (file, status)

  -- We only need to check our last two seasons, as only one may not yet be known (committed)
  statuses <- mapM toTuple (take 2 seasons)

  case filter (\(_, s) -> s == Committed) statuses of
    [] -> pure Nothing -- No known seasons
    (f,s):xs -> pure (Just f) -- Latest known commited season


-- findlatestSeasonFile :: IO (Maybe Turtle.FilePath)
-- findlatestSeasonFile = do
--   seasons <- seasonFiles
--   pure $ headMay seasons


checkExistingSeason :: IO (Turtle.FilePath, EvergreenStatus)
checkExistingSeason = do
  seasons <- seasonFiles

  case seasons of
    [] -> pure ("", Uninitiated) -- No seasons exist
    seasonFile:xs -> do
      status <- gitStatus seasonFile

      case status of
        -- Seasons exist
        x -> pure (seasonFile, x) -- Latest season status
        -- Committed -> pure (seasonFile, Committed) -- But the latest one is commited
        -- _ -> case xs of -- @TODO what's going on here? Why do we need to check the prior file again...?
        --   [] -> pure (seasonFile, ChangesPending)
        --   y:ys -> do
        --     status' <- gitStatus y
        --     pure (seasonFile, status')



gitStatus :: Turtle.FilePath -> IO EvergreenStatus
gitStatus filepath = do

  let p = asText filepath

  print $ "Gitstatus for..." <> p
  gsPorcelain <- strict $ inshell ("git status --porcelain " <> p) empty
  print $ firstTwo gsPorcelain
  case firstTwo gsPorcelain of
    ('_','_')       -> do
      -- `git status` is empty, so we need to check if the file is tracked (thus clean) or non-existent
      gitFiles <- strict $ inshell ("git ls-files " <> p) empty
      case gitFiles of
        "" -> pure Uninitiated
        _ -> pure Committed

    ('?', _)   -> pure ChangesPending -- Untracked, but exists
    ('A', _)   -> pure ChangesPending -- Added, so not yet committed
    (' ', 'M') -> pure ChangesPending -- Modified
    ('M', ' ') -> pure ChangesPending -- Added, so not yet committed

    (' ', 'D') -> pure Deleted -- Added, so not yet committed


    -- @TODO other statuses other than Uninitiatied
    -- https://git-scm.com/docs/git-status#_short_format
    _ ->  pure UnexpectedEvergreenStatus


firstTwo :: Text -> (Char, Char)
firstTwo text = (first, second)
  where
    part   = snd <$> T.uncons text
    first  = fromMaybe '_' $ fst <$> T.uncons text
    second = fromMaybe '_' $ fst <$> (T.uncons =<< part)


data EvergreenStatus
  = Uninitiated
  | ChangesPending
  | Committed
  | Staged
  | Deleted
  | UnexpectedEvergreenStatus
  deriving (Show, Eq)


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


loadSchemaAst :: String -> IO Module
loadSchemaAst modelVersion = fromParseResult <$> parseFile ("types/" ++ modelVersion ++ ".hs")


loadFileAst :: Turtle.FilePath -> IO Module
loadFileAst filepath = fromParseResult <$> parseFile (T.unpack . asText $ filepath)


writeSeasonAst :: Turtle.FilePath -> Module -> IO ()
writeSeasonAst filename ast = writeTextFile filename $ T.pack $ prettyPrint ast

-- writeTextFile seasonFile $ T.pack $ prettyPrint $ astModel "Model_A" []


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
-- @TODO Need to transform the Maybe here
fieldDecltoField (FieldDecl (Ident name:xs) (TyCon (UnQual (Ident tipe)))) = (name, tipe, False)
fieldDecltoField _ = ("Error:fieldDecltoField", "Field doens't match shape", False)


areDataDecls :: Decl -> Decl -> Bool
areDataDecls DataDecl{} DataDecl{} = True
areDataDecls _        _            = False



createTable :: Hilt.Handles.Postgres.Handle -> String -> IO ()
createTable db table = do
  Hilt.Postgres.execute db (tryCreateTableStmt (S8.pack table)) ()
  pure ()


tryCreateTableStmt :: S8.ByteString
                  -- ^ Table name
                  -> SQL.Query
tryCreateTableStmt tableName =
  Query $ S8.concat [ "create table if not exists ", quoteIdent tableName, " ();"]


addColumn :: Hilt.Handles.Postgres.Handle -> String -> String -> String -> IO ()
addColumn db table column tipe = do
  Hilt.Postgres.execute db (add_column_stmt (S8.pack table) (S8.pack column) (S8.pack tipe)) ()
  pure ()


removeColumn :: Hilt.Handles.Postgres.Handle -> String -> String -> IO ()
removeColumn db table column = do
  Hilt.Postgres.execute db (drop_column_stmt (S8.pack table) (S8.pack column)) ()
  pure ()




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

fileAstSha :: Turtle.FilePath -> IO Text
fileAstSha file = do
  ast <- loadFileAst file
  pure $ astSha ast


astSha :: Module -> Text
astSha ast = tShow $ sha1 $ tShow ast


sha1 :: Text -> Digest SHA1
sha1 = hash . E.encodeUtf8


asText :: Turtle.FilePath -> Text
asText = format fp


tShow :: Show a => a -> Text
tShow = T.pack . show


doNothing :: IO ()
doNothing = pure ()


gs :: MonadIO io => io ()
gs = stdout $ inshell "git status" empty
