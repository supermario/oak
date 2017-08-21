{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Database.PostgreSQL.Simple as SQL
import qualified Hilt
import qualified Hilt.Postgres
import HiltPostgres -- @TODO remove me

import Language.Haskell.Exts.Simple
import Data.List ((\\), find)
import Data.Maybe (fromMaybe)

import Data.Time (getCurrentTime, defaultTimeLocale, formatTime)

import Data.Char (toLower)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Data.Int
import Turtle
import Filesystem.Path.CurrentOS (fromText)
import qualified Control.Foldl as Fold
import Safe

import Data.Text.Encoding as E
import Crypto.Hash


-- Internal
import Migrations


-- @TODO read from package.yaml using hpack lib
applicationName :: String
applicationName = "oak"


migrate :: IO ()
migrate =
  Hilt.once $ do

    -- @TODO check if the DB exists and be more helpful
    db <- Hilt.Postgres.load


    Hilt.program $ do
      -- @TODO Temporary while we're testing
      dropTable db "user"

      dbInfo <- Hilt.Postgres.dbInfo db
      Hilt.Postgres.pp dbInfo

      case dbInfo of
        [] -> do
          putStrLn "Database is empty, initialising."
          displayOrRun "init" db

        _ -> do
          print $ tShow $ dbInfoToAst dbInfo
          let dbSha = tShow $ sha1 $ tShow $ dbInfoToAst dbInfo

          T.putStrLn $ "Running migration for DB SHA: " <> dbSha
          displayOrRun dbSha db


displayOrRun :: Text -> Hilt.Postgres.Handle -> IO ()
displayOrRun version db =
  case migrationsFor version db of
    Left err -> do
      putStrLn err
      showDbDiff db

    Right migrations -> do
      sequence_ migrations
      pure ()


main :: IO ()
main = Hilt.once $ do

  -- @TODO check if the DB exists and be more helpful
  db <- Hilt.Postgres.load

  Hilt.program $ do

    bootstrapEvergreen

    -- @TODO how will we do this dynamically?
    schemaAst <- loadSchemaAst "Schema"

    let schemaSha = tShow $ sha1 $ tShow schemaAst

    newSeasonFile <- newSeasonFile schemaSha
    schemaStatus <- gitStatus $ fromText "types/Schema.hs"

    -- There may or may not already be a season to compare to.
    (seasonFile, seasonStatus) <- checkExistingSeason

    let writeAndShowSeason = do
          seasonChanges <- getSeasonChanges newSeasonFile schemaAst
          -- @TODO migrations need to be fixed up here
          -- writeSeasonAst newSeasonFile (addMigrations schemaAst seasonChanges)
          writeSeasonAst newSeasonFile schemaAst
          showSeasonChanges newSeasonFile seasonChanges

    case schemaStatus of
      ChangesPending ->
        -- Our Schema.hs has changes to be committed.

        case seasonStatus of
          Uninitiated -> do
            -- There are no prior seasons yet, create our first one
            T.putStrLn $ "Writing first season to " <> asText newSeasonFile
            writeAndShowSeason

          ChangesPending -> do
            -- A season with pending changes exists (unfinished season) so update it
            T.putStrLn $ "Updating season " <> asText seasonFile

            -- Always remove the currently uncomitted season file to keep as close as possible
            -- to the commit timestamp, as well as always have the correct AST SHA
            rm seasonFile
            writeAndShowSeason

          Committed -> do
            seasonSha <- fileAstSha seasonFile

            if schemaSha == seasonSha then
              T.putStrLn "Schema has not been commited, but already matches a known season. This is bad... you forgot to commit your Schema!"
            else do
              T.putStrLn $ "Writing next season to " <> asText newSeasonFile
              writeAndShowSeason


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



addMigrations :: Module -> SeasonChanges -> Module
addMigrations (Module mHead mPragmas mImports mDecls) changes =
  -- @TODO adjust import header here?
  Module mHead mPragmas mImports (mDecls <> migrationAst changes)

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


bootstrapEvergreen :: IO ()
bootstrapEvergreen = mktree "evergreen/seasons"


type SeasonChanges = (Turtle.FilePath, String, EvergreenRecordStatus, [Diff])

getSeasonChanges :: Turtle.FilePath -> Module -> IO SeasonChanges
getSeasonChanges seasonFile schemaAst = do
  lastKnownM <- findLastKnownSeason

  let schemaDecl = moduleDataDecl schemaAst
      recordName = dataDeclName schemaDecl
      empty = moduleDataDecl $ astModel "Schema" "Model" []

  case lastKnownM of
    Nothing ->
      -- There are no prior seasons. We have a creation event.
      pure (seasonFile, recordName, New, diff empty schemaDecl)

    Just lastKnownSeason ->
      if lastKnownSeason == seasonFile then do
        T.putStrLn "The last known season is the same as the new season, which seems impossible. This seems like it must be a bug."
        pure (seasonFile, recordName, New, diff empty schemaDecl)
      else do
        lastKnownAst <- loadFileAst lastKnownSeason

        let lastDecl   = moduleDataDecl lastKnownAst

        pure (seasonFile, recordName, Updated, diff lastDecl schemaDecl)


showSeasonChanges :: Turtle.FilePath -> SeasonChanges -> IO ()
showSeasonChanges seasonFile (filepath, recordName, recordStatus, diff) =
  showSeasonDiff filepath recordName recordStatus diff


showSeasonDiff :: Turtle.FilePath -> String -> EvergreenRecordStatus -> [Diff] -> IO ()
showSeasonDiff seasonFile recordName recordStatus changes = do
  T.putStrLn ""
  T.putStrLn ""
  T.putStrLn ""
  T.putStrLn "Schema changes to be committed:"
  T.putStrLn $ "  (season remembered in " <> asText seasonFile <> ")"
  T.putStrLn ""
  T.putStrLn $ "  " <> T.pack (show recordStatus) <> " record: " <> T.pack recordName
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
  pure $ fromText $ "evergreen/seasons/Schema_" <> T.pack timestamp <> "_" <> sha <> ".hs"


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


data EvergreenRecordStatus
  = New
  | Updated
  deriving (Show, Eq)


dbStatus :: Hilt.Postgres.DbInfo -> Module -> DbStatus
dbStatus dbInfo ast = do

  let dbAst    = dbInfoToAst dbInfo
      modelAst = ast

  if dbAst == modelAst then Clean else Dirty


dbInfoToAst :: Hilt.Postgres.DbInfo -> Module -- @TODO Maybe Module
dbInfoToAst dbInfo = do
  let
    target = "user" -- @HARDCODED paramaterise this later
    predicate Hilt.Postgres.TableInfo{..} = tableName == target

  case Data.List.find predicate dbInfo of
    Just Hilt.Postgres.TableInfo{..} ->
      astModel "Schema" "User" (fmap fieldInfoToField fields)

    Nothing -> astModel "Schema" "X" []

  -- @TODO cover if database is empty case


fieldInfoToField :: Hilt.Postgres.FieldInfo -> Field
fieldInfoToField Hilt.Postgres.FieldInfo{..} = (fieldName, tipe, fieldNullable)

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


loadSchemaAst :: String -> IO Module
loadSchemaAst modelVersion =
  fromParseResult <$> parseFile ("types/" ++ modelVersion ++ ".hs")


loadFileAst :: Turtle.FilePath -> IO Module
loadFileAst filepath =
  fromParseResult <$> parseFile (T.unpack . asText $ filepath)


writeSeasonAst :: Turtle.FilePath -> Module -> IO ()
writeSeasonAst filename ast =
  writeTextFile filename $ T.pack $ prettyPrint ast

-- writeTextFile seasonFile $ T.pack $ prettyPrint $ astModel "Model_A" []


astModel :: String -> String -> [Field] -> Module
astModel moduleName recordName fields = do
  let
    fieldDecls = fmap (\(name, tipe, _) ->
        -- @TODO maybe on bool?
        FieldDecl [Ident name] (TyCon (UnQual (Ident tipe)))
      ) fields


  Module (Just (ModuleHead (ModuleName moduleName) Nothing Nothing)) [] []
    [
      DataDecl
        DataType Nothing (DHead (Ident recordName))
        [QualConDecl Nothing Nothing (RecDecl (Ident recordName) fieldDecls)]
        Nothing
    ]

-- @TODO make this typesafe
-- (name, type, nullable)
type Field = (String, String, Bool)

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


dataDeclName :: Decl -> String
dataDeclName (DataDecl _ _ _ (QualConDecl Nothing Nothing (RecDecl (Ident name) _):xs) _)
  = name
dataDeclName d = "Error: Decl is not a DataDecl type: " ++ show d


migrationAst :: SeasonChanges -> [Decl]
migrationAst (seasonPath, recordName, recordStatus, changes) =
  [ TypeSig [Ident "migration_X_Y"] (TyList (TyCon (UnQual (Ident "Migration"))))
  , PatBind
      (PVar (Ident "migration_X_Y"))
      (UnGuardedRhs
         (List (fmap (changeAst recordName) changes)))
      Nothing
  ]


changeAst :: String -> Diff -> Exp
changeAst recordName change =
  case change of
    Added (name, tipe, nullable) ->
      App
        (App (App (Con (UnQual (Ident "AddColumn"))) (Lit (String $ lowercase recordName)))
           (Lit (String name)))
        (Lit (String $ sqlType tipe))
    Removed (name, tipe, nullable) ->
      App (App (Con (UnQual (Ident "RemoveColumn"))) (Lit (String $ lowercase recordName)))
        (Lit (String name))
    Debug debugString ->
      App (App (Con (UnQual (Ident "Debug"))) (Lit (String "thisIsAnError")))
        (Lit (String debugString))


sqlType :: String -> String
sqlType tipe =
  -- @TODO handle nullable
  case tipe of
    "String" -> "varchar(255) not null"
    _ -> "ERROR UNKNOWN TYPE"



showDbDiff db = do
  schemaAst <- loadSchemaAst "Schema"
  dbInfo <- Hilt.Postgres.dbInfo db

  print schemaAst
  print $ dbInfoToAst dbInfo


  let
    dbModelAst = moduleDataDecl $ dbInfoToAst dbInfo
    codeModelAst = moduleDataDecl schemaAst

  putStrLn "The differences between the DB and the latest Schema are:"
  print $ diff dbModelAst codeModelAst



-- TESTS

testSample :: IO ()
testSample = prettyPrint . fromParseResult <$> parseFile "evergreen/seasons/Schema_20170812141450_be119f5af8585265f8a03acda4d86dfe6eaecb22.hs" >>= putStrLn


testAst :: IO ()
testAst = putStrLn $ prettyPrint $ astModel "Schema" "ModelA" []


testParse :: IO (ParseResult Module)
testParse = parseFile "evergreen/seasons/Schema_20170812141450_be119f5af8585265f8a03acda4d86dfe6eaecb22.hs"


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


lowercase :: String -> String
lowercase = map toLower