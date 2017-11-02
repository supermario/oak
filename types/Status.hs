{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Database.PostgreSQL.Simple as SQL
import qualified Hilt
import qualified Hilt.Postgres
import HiltPostgres -- @TODO remove me when done with DB mocking tests

import Language.Haskell.Exts.Simple
import Data.List ((\\), find)
import Data.Maybe (fromMaybe)

import Data.Time (getCurrentTime, defaultTimeLocale, formatTime)

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Data.Int
import Turtle
import Filesystem.Path.CurrentOS (fromText)
import qualified Control.Foldl as Fold
import Safe

import Data.Function ((&))

import MigrationHelpers (migrationExists, migrationsFor)
import MigrationsAst
import ShellHelpers

-- Internal
-- @TODO this is a link into the real project... we'll have to invert this to
-- be injected instead...? How do we run status then?
import Migrations (allMigrations)


main :: IO ()
main = putStrLn "Nope!"


-- @TODO read from package.yaml using hpack lib
applicationName :: String
applicationName = "oak"


migrate :: IO ()
migrate = Hilt.once $ do

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
        -- Look at the DB schema and generate a SHA that identifies its AST
        let dbSha = tShow $ sha1 $ tShow $ dbInfoToAst dbInfo

        -- Check if this SHA is a known season amongst our current migrations
        if migrationExists allMigrations dbSha
          -- Ok, lets go ahead and try to process the migration from the current SHA onwards
          then displayOrRun dbSha db
          else do
            -- @TODO How can we make these messages more user friendly?
            T.putStrLn $ "Error: database current state of " <> dbSha <> " does not match any known seasons."
            T.putStrLn "It's not safe to proceed, so I'm bailing out!"
            -- @TODO show a diff of DB schema vs Schema file?

-- Something like
-- Hilt.evergreen $ do
-- Could do the magic to handle this under the hood
-- Or perhaps even better, an Evergreen version of the postgres service, that does this on load?
-- db <- Hilt.Evergreen.load


displayOrRun :: Text -> Hilt.Postgres.Handle -> IO ()
displayOrRun dbSha db = case migrationsFor allMigrations dbSha db of
  Left err -> do
    T.putStrLn err
    showDbDiff db

  Right migrations -> do
    T.putStrLn $ "Fetching migrations for DB SHA: " <> dbSha
    sequence_ migrations
    pure ()


status :: IO ()
status = Hilt.once $ do

  -- @TODO check if the DB exists and be more helpful
  db <- Hilt.Postgres.load

  Hilt.program $ do

    bootstrapEvergreen

    -- @TODO how will we do this dynamically?
    schemaAst <- loadSchemaAst "Schema"
    let schemaSha = tShow $ sha1 $ tShow schemaAst
    schemaStatus  <- gitStatus $ fromText "types/Schema.hs"

    newSeasonFile <- newSeasonFile schemaSha
    let newSeasonFilepath = fromText $ "evergreen/seasons/" <> newSeasonFile <> ".hs"


    -- There may or may not already be a season to compare to.
    (seasonFile, seasonStatus) <- checkExistingSeason

    let writeAndShowSeason = do
          seasonChanges <- getSeasonChanges newSeasonFilepath schemaAst
          -- @TODO fixme, crap formatting
          writeSeasonAst newSeasonFilepath $ adjustModuleName newSeasonFile (addMigrations schemaAst seasonChanges)
          showSeasonChanges newSeasonFilepath seasonChanges

    case schemaStatus of
      -- Our Schema.hs has changes to be committed.
      ChangesPending -> case seasonStatus of
        Uninitiated -> do
          -- There are no prior seasons yet, create our first one
          T.putStrLn $ "Writing first season to " <> asText newSeasonFilepath
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

          if schemaSha == seasonSha
            then
              T.putStrLn
                "Schema has not been commited, but already matches a known season. This is bad... you forgot to commit your Schema!"
            else do
              T.putStrLn $ "Writing next season to " <> asText newSeasonFilepath
              writeAndShowSeason

        x -> do
          -- This shows the changes between last season, and the current Schema
          -- Changes are written to a new, uncommited season file
          -- showSeasonChanges
          T.putStrLn "What next?"
          print x
          -- showFirstSeason

      Uninitiated -> do
        putStrLn "Schema not found!"
        putStrLn "I was looking for types/Schema.hs, but could not find it."
        -- No Schema.hs file exists... should we write a new one?

      Committed -> do
        -- @TODO even though it's in season, it might not match. Need to diff SHA and test integrity?
        -- @TODO there are no known seasons and this says it's all good if we're committed
        putStrLn "Schema has no new changes.\n"
        putStrLn "@TODO have not implemented seasons check - might be dirty seasons because you're fiddling\n"
        T.putStrLn $ "newSeasonFilepath:" <> asText newSeasonFilepath

        -- @TODO also – fix the (status, +field, status, -field, status) flow - season should be removed
        -- What happens is the migration that's created at (+field) then is not known to be removed if the change is reverted
        -- we end up in this block, because the Schema is already committed (changes we made temporarily were undone)
        -- So we don't know to remove the other files...? However in `Migrations.hs` we've got the new migration sitting.
        -- Things probably need to get rebuilt here too, regardless of Schema status, to retain integrity.

        -- Lets pretend for now Oak handles commits and they never go wrong, we can just throw away dirty stuff if we're "back to committed"
        -- @NOTE because this executes last, all our debugging above will show "incorrect" stuff that's suddenly gone after these lines
        _ <- shellExec "rm -rf evergreen/seasons && git checkout HEAD -- evergreen/seasons"
        _ <- shellExec "git checkout HEAD -- evergreen/Migrations.hs"
        pure ()


      Deleted -> do
        putStrLn "It looks like types/Schema.hs has been deleted, which seems really bad..."
        putStrLn "Perhaps you want to `git checkout types/Schema.hs` to restore it?"

      UnexpectedEvergreenStatus -> putStrLn "Got an unexpected Evergreen status... please check `evergreenStatus`"

    pure ()

    writeMigrationsAst

    -- So now, :migrate will happily migrate if it finds an old SHA - however we may not yet
    -- have commited the Schema! As Migrations.hs is autogenerated, this (might?) be a problem.
    -- So we'll likely need to add logic to:
    --   - Check up to which SHA we've commited, and only migrate the gaps?
    --   - What if we want to test while we're developing, but without commitment yet?




addMigrations :: Module -> SeasonChanges -> Module
addMigrations (Module mHead mPragmas mImports mDecls) changes =
  -- @TODO adjust import header here?
                                                                Module mHead
                                                                       mPragmas
                                                                       importDecl
                                                                       (mDecls <> migrationsAst changes)
 where
  importDecl =
    [ ImportDecl
        { importModule    = ModuleName "HiltPostgres"
        , importQualified = False
        , importSrc       = False
        , importSafe      = False
        , importPkg       = Nothing
        , importAs        = Nothing
        , importSpecs     = Nothing
        }
    ]


adjustModuleName :: Text -> Module -> Module
adjustModuleName filename (Module mHead mPragmas mImports mDecls) =
  Module (Just (ModuleHead (ModuleName (T.unpack filename)) Nothing Nothing)) mPragmas mImports mDecls


bootstrapEvergreen :: IO ()
bootstrapEvergreen = mktree "evergreen/seasons"


type SeasonChanges = (Turtle.FilePath, String, EvergreenRecordStatus, [Diff])

getSeasonChanges :: Turtle.FilePath -> Module -> IO SeasonChanges
getSeasonChanges seasonFile schemaAst = do
  lastKnownM <- findLastKnownSeason

  let schemaDecl = moduleDataDecl schemaAst
      recordName = dataDeclName schemaDecl
      empty      = moduleDataDecl $ astModel "Schema" "Model" []

  case lastKnownM of
    Nothing ->
      -- There are no prior seasons. We have a creation event.
      pure (seasonFile, recordName, New, diff empty schemaDecl)

    Just lastKnownSeason -> if lastKnownSeason == seasonFile
      then do
        T.putStrLn
          "The last known season is the same as the new season, which seems impossible. This seems like it must be a bug."
        pure (seasonFile, recordName, New, diff empty schemaDecl)
      else do
        lastKnownAst <- loadFileAst lastKnownSeason

        let lastDecl = moduleDataDecl lastKnownAst

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
  T.putStrLn $ "  " <> T.pack recordName <> " record will be " <> T.pack (lowercase $ show recordStatus)
  T.putStrLn ""
  mapM_ (putStrLn . toSeasonDiffText) changes
  T.putStrLn ""


toSeasonDiffText :: Diff -> String
toSeasonDiffText change = case change of
    -- @TODO respect bool
  Added   (name, tipe, bool) -> "        added:   " <> name <> " :: " <> tipe
  Removed (name, tipe, bool) -> "        removed: " <> name <> " :: " <> tipe


newSeasonFile :: Text -> IO Text
newSeasonFile sha = do
  currentTime <- getCurrentTime

  let timestamp = formatTime defaultTimeLocale "%Y%m%d%H%M%S" currentTime

  pure $ "Schema_" <> T.pack timestamp <> "_" <> sha


seasonFiles :: IO [Turtle.FilePath]
seasonFiles = fold (Turtle.find (suffix ".hs") "evergreen/seasons") Fold.revList


seasonFileWithSha :: Text -> IO [Turtle.FilePath]
seasonFileWithSha sha = fold (Turtle.find (contains $ text sha) "evergreen/seasons") Fold.revList


findLastKnownSeason :: IO (Maybe Turtle.FilePath)
findLastKnownSeason = do
  seasons <- seasonFiles

  let toTuple file = do
        status <- gitStatus file
        pure (file, status)

  -- We only need to check our last two seasons, as only one may not yet be known (committed)
  statuses <- mapM toTuple (take 2 seasons)

  case filter (\(_, s) -> s == Committed) statuses of
    [] ->
      pure Nothing -- No known seasons
    (f, s):xs -> pure (Just f) -- Latest known commited season


checkExistingSeason :: IO (Turtle.FilePath, EvergreenStatus)
checkExistingSeason = do
  seasons <- seasonFiles

  case seasons of
    [] ->
      pure ("", Uninitiated) -- No seasons exist
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
  gsPorcelain <- shellExec $ "git status --porcelain " <> p
  print $ firstTwo gsPorcelain
  case firstTwo gsPorcelain of
    ('_', '_') -> do
      -- `git status` is empty, so we need to check if the file is tracked (thus clean) or non-existent
      gitFiles <- shellExec $ "git ls-files " <> p
      case gitFiles of
        "" -> pure Uninitiated
        _  -> pure Committed

    ('?', _) ->
      pure ChangesPending -- Untracked, but exists
    ('A', _) ->
      pure ChangesPending -- Added, so not yet committed
    (' ', 'M') ->
      pure ChangesPending -- Modified
    ('M', ' ') ->
      pure ChangesPending -- Added, so not yet committed
    (' ', 'D') ->
      pure Deleted -- Added, so not yet committed


    -- @TODO other statuses other than Uninitiatied
    -- https://git-scm.com/docs/git-status#_short_format
    _ -> pure UnexpectedEvergreenStatus


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

data DbStatus
  = Clean
  | Pending
  | Dirty
  deriving (Show)


loadSchemaAst :: String -> IO Module
loadSchemaAst modelVersion = fromParseResult <$> parseFile ("types/" ++ modelVersion ++ ".hs")




writeSeasonAst :: Turtle.FilePath -> Module -> IO ()
writeSeasonAst filename ast = writeTextFile filename $ T.pack $ prettyPrint ast

-- writeTextFile seasonFile $ T.pack $ prettyPrint $ astModel "Model_A" []


astModel :: String -> String -> [Field] -> Module
astModel moduleName recordName fields = do
  let fieldDecls =
        -- @TODO handle Maybe types on bool?
        fmap (\(name, tipe, _) -> FieldDecl [Ident name] (TyCon (UnQual (Ident tipe)))) fields

  Module
    (Just (ModuleHead (ModuleName moduleName) Nothing Nothing))
    []
    []
    [ DataDecl DataType
               Nothing
               (DHead (Ident recordName))
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
diff d1 d2 = if areDataDecls d1 d2
  then do
    let f1      = fieldDecs d1
        f2      = fieldDecs d2
        added   = Added <$> (f2 \\ f1)
        removed = Removed <$> (f1 \\ f2)

    added ++ removed
  else [Debug "Cannot compare structures that aren't data declarations"]


moduleDataDecl :: Module -> Decl
moduleDataDecl (Module (Just (ModuleHead (ModuleName name) Nothing Nothing)) _ _ (dataDecl:xs)) = dataDecl
moduleDataDecl _ = undefined -- @TODO Fix me with a nicer error!


fieldDecs :: Decl -> [Field]
fieldDecs (DataDecl _ _ _ (QualConDecl Nothing Nothing (RecDecl (Ident _) fieldDecls):xs) _) =
  fmap fieldDecltoField fieldDecls
fieldDecs d = [("Error:fieldDecs", "Field is not a DataDecl with QualConDecl", False)]


fieldDecltoField :: FieldDecl -> Field
-- @TODO Need to transform the Maybe here
fieldDecltoField (FieldDecl (Ident name:xs) (TyCon (UnQual (Ident tipe)))) = (name, tipe, False)
fieldDecltoField _ = ("Error:fieldDecltoField", "Field doens't match shape", False)


areDataDecls :: Decl -> Decl -> Bool
areDataDecls DataDecl{} DataDecl{} = True
areDataDecls _          _          = False


dataDeclName :: Decl -> String
dataDeclName (DataDecl _ _ _ (QualConDecl Nothing Nothing (RecDecl (Ident name) _):xs) _) = name
dataDeclName d = "Error: Decl is not a DataDecl type: " ++ show d


migrationsAst :: SeasonChanges -> [Decl]
migrationsAst (seasonPath, recordName, recordStatus, changes) =
  [FunBind [Match (Ident "migration") [PVar (Ident "db")] (UnGuardedRhs (Do migrations)) Nothing]]
  where migrations = fmap (migrationStmt recordName) changes


migrationStmt :: String -> Diff -> Stmt
migrationStmt recordName change = case change of
  Added (name, tipe, nullable) -> Qualifier
    ( App
      ( App
        (App (App (Var (UnQual (Ident "addColumn"))) (Var (UnQual (Ident "db")))) (Lit (String (lowercase recordName))))
        (Lit (String name))
      )
      (Lit (String $ sqlType name tipe))
    )

  Removed (name, tipe, nullable) -> Qualifier
    ( App
      ( App (App (Var (UnQual (Ident "removeColumn"))) (Var (UnQual (Ident "db"))))
            (Lit (String (lowercase recordName)))
      )
      (Lit (String name))
    )

  Debug debugString -> Qualifier
    ( App
      (App (App (Var (UnQual (Ident "ERROR"))) (Var (UnQual (Ident "ERROR")))) (Lit (String (lowercase recordName))))
      (Lit (String debugString))
    )


-- @TODO handle nullable
sqlType :: String -> String -> String
sqlType name tipe = case name of
  "id" -> "serial primary key"
  _    -> case tipe of
    "String" -> "varchar(255) not null"
    _        -> "ERROR UNKNOWN TYPE"


showDbDiff :: Hilt.Postgres.Handle -> IO ()
showDbDiff db = do
  schemaAst <- loadSchemaAst "Schema"
  dbInfo    <- Hilt.Postgres.dbInfo db

  print schemaAst
  print $ dbInfoToAst dbInfo

  let dbModelAst   = moduleDataDecl $ dbInfoToAst dbInfo
      codeModelAst = moduleDataDecl schemaAst

  putStrLn "The differences between the DB and the latest Schema are:"
  print $ diff dbModelAst codeModelAst


-- TESTS

testSample :: IO ()
testSample = prettyPrint . fromParseResult <$> parseFile "evergreen/seasons/Migrations.hs" >>= putStrLn


testAst :: IO ()
testAst = putStrLn $ prettyPrint $ astModel "Schema" "ModelA" []


testParse :: IO (ParseResult Module)
testParse = parseFile "evergreen/seasons/Schema_20170812141450_be119f5af8585265f8a03acda4d86dfe6eaecb22.hs"


testWriteAst :: IO ()
testWriteAst = do
  ast <- parseFile "evergreen/Migrations.hs"
  writeTextFile "formattedAst.hs" $ T.pack $ show ast
  stdout $ inshell "hindent --style gibiansky formattedAst.hs" empty
  pure ()


writeMigrationsAst :: IO ()
writeMigrationsAst = do
  allSeasonFiles <- seasonFiles
  let seasonFiles = fmap asText allSeasonFiles
      ast         = migrationAst seasonFiles
  writeTextFile "evergreen/Migrations.hs" $ T.pack $ prettyPrint ast
  pure ()


--- Mocks

{-

name        varchar(255) NOT NULL
title       varchar(40) NOT NULL
did         integer NOT NULL,
date_prod   date,
kind        varchar(10),
len         interval hour to minute

-}
