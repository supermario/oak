{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Hilt
import qualified Hilt.Postgres
import HiltPostgres -- @TODO remove me when done with DB mocking tests

import Language.Haskell.Exts.Simple
import Data.List ((\\))
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Dict
import Data.Time (getCurrentTime, defaultTimeLocale, formatTime)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Turtle
import Filesystem.Path.CurrentOS (fromText)
import qualified Control.Foldl as Fold

import MigrationHelpers (migrationExists, migrationsFor)
import AstMigrations
import AstMigration (SeasonChanges, Field, Diff(..), RecordChanges, EvergreenRecordStatus(..), addMigrations, showSeasonChanges)
import AstDatabase (dbInfoToAst)
import AstHelpers
import ShellHelpers


-- Internal
-- @TODO this is a link into the real project... we'll have to invert this to
-- be injected instead...? How do we run status then?
import Migrations (allMigrations)


parser :: Parser Text
parser = argText "<command>" "One of: status, migrate"


main :: IO ()
main = do
  cmd <- options "Evergreen migrations" parser
  case cmd of
    "status"  -> status
    "migrate" -> migrate
    _         -> T.putStrLn $ "Unsupported command: " <> cmd


-- @TODO read from package.yaml using hpack lib
applicationName :: String
applicationName = "oak"


migrate :: IO ()
migrate = Hilt.once $ do

    -- @TODO check if the DB exists and be more helpful
  db <- Hilt.Postgres.load

  Hilt.program $ do
    -- @TODO Temporary while we're testing
    -- @TODO can we have one command to drop all tables in DB?
    dropTable db "user"
    dropTable db "account"

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
  Left err_ -> do
    T.putStrLn err_
    showDbDiff db

  Right migrations -> do
    T.putStrLn $ "Fetching migrations for DB SHA: " <> dbSha
    sequence_ migrations
    pure ()


status :: IO ()
status = Hilt.once $ do

  -- @TODO check if the DB exists and be more helpful
  -- db <- Hilt.Postgres.load
  pure () -- Keep formatting at bay...

  Hilt.program $ do

    bootstrapEvergreen

    -- @TODO how will we do this dynamically?
    schemaAst <- loadSchemaAst "Schema"
    let schemaSha = tShow $ sha1 $ tShow schemaAst
    schemaStatus      <- gitStatus $ fromText "types/Schema.hs"

    newSeasonFilename <- newSeasonFile schemaSha
    let newSeasonFilepath = fromText $ "evergreen/seasons/" <> newSeasonFilename <> ".hs"

    -- There may or may not already be a season to compare to.
    (seasonFile, seasonStatus) <- checkExistingSeason

    case schemaStatus of
      Uninitiated -> do
        putStrLn "Schema not found!"
        putStrLn "I was looking for types/Schema.hs, but could not find it."
        -- No Schema.hs file exists... should we write a new one?

      Deleted -> do
        putStrLn "It looks like types/Schema.hs has been removed!"
        putStrLn "Perhaps you want to `git checkout types/Schema.hs` to restore it?"

      UnexpectedEvergreenStatus -> putStrLn "Got an unexpected Evergreen status... please check `evergreenStatus`"

      Committed                 -> do
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
        -- _ <- shellExec "rm -rf evergreen/seasons && git checkout HEAD -- evergreen/seasons"
        -- _ <- shellExec "git checkout HEAD -- evergreen/Migrations.hs"
        pure ()

      -- Our Schema.hs has changes to be committed.
      ChangesPending ->
        checkAndUpdateSeason seasonStatus seasonFile newSeasonFilepath schemaSha schemaAst newSeasonFilename

      _ -> checkAndUpdateSeason seasonStatus seasonFile newSeasonFilepath schemaSha schemaAst newSeasonFilename

    pure ()

    writeMigrationsSummaryFile

    -- So now, :migrate will happily migrate if it finds an old SHA - however we may not yet
    -- have commited the Schema! As Migrations.hs is autogenerated, this (might?) be a problem.
    -- So we'll likely need to add logic to:
    --   - Check up to which SHA we've commited, and only migrate the gaps?
    --   - What if we want to test while we're developing, but without commitment yet?


checkAndUpdateSeason :: EvergreenStatus -> Turtle.FilePath -> Turtle.FilePath -> Text -> Module -> Text -> IO ()
checkAndUpdateSeason seasonStatus seasonFile newSeasonFilepath schemaSha schemaAst newSeasonFilename =
  case seasonStatus of
    Uninitiated -> do
      -- There are no prior seasons yet, create our first one
      T.putStrLn $ "Writing first season to " <> asText newSeasonFilepath
      writeAndShowSeason newSeasonFilepath schemaAst newSeasonFilename

    ChangesPending -> do
      -- A season with pending changes exists (unfinished season) so update it
      T.putStrLn $ "Updating season " <> asText seasonFile

      -- Always remove the currently uncomitted season file to keep as close as possible
      -- to the commit timestamp, as well as always have the correct AST SHA
      rm seasonFile
      writeAndShowSeason newSeasonFilepath schemaAst newSeasonFilename

    Committed -> do
      seasonSha <- fileAstSha seasonFile

      if schemaSha == seasonSha
        then
          T.putStrLn
            "Schema has not been commited, but already matches a known season. This is bad... you forgot to commit your Schema!"
        else do
          T.putStrLn $ "Writing next season to " <> asText newSeasonFilepath
          writeAndShowSeason newSeasonFilepath schemaAst newSeasonFilename

    other -> do
      -- This shows the changes between last season, and the current Schema
      -- Changes are written to a new, uncommited season file
      -- showSeasonChanges
      T.putStrLn "What next?"
      print other
      -- showFirstSeason


writeAndShowSeason :: Turtle.FilePath -> Module -> Text -> IO ()
writeAndShowSeason newSeasonFilepath schemaAst newSeasonFilename = do
  seasonChanges <- getSeasonChanges newSeasonFilepath schemaAst
  -- @TODO fixme, crap formatting
  writeSeasonAst newSeasonFilepath $ adjustModuleName newSeasonFilename (addMigrations schemaAst seasonChanges)
  showSeasonChanges newSeasonFilepath seasonChanges


adjustModuleName :: Text -> Module -> Module
adjustModuleName fileName_ (Module _ mPragmas mImports mDecls) =
  Module (Just (ModuleHead (ModuleName (T.unpack fileName_)) Nothing Nothing)) mPragmas mImports mDecls
adjustModuleName _ module_ = module_ -- Failure case @TODO should it be silent?


bootstrapEvergreen :: IO ()
bootstrapEvergreen = mktree "evergreen/seasons"


getSeasonChanges :: Turtle.FilePath -> Module -> IO SeasonChanges
getSeasonChanges seasonFile schemaAst = do
  lastKnownM <- findLastKnownSeason

  let schemaDecls = moduleDataDecls schemaAst
      emptySchema = moduleDataDecls $ astModel "Schema" "Model" []

  case lastKnownM of
    Nothing ->
      -- There are no prior seasons. We have a creation event.
      pure (seasonFile, diff emptySchema schemaDecls)

    Just lastKnownSeason -> if lastKnownSeason == seasonFile
      then do
        T.putStrLn
          "The last known season is the same as the new season, which seems impossible. This seems like it must be a bug."
        pure (seasonFile, diff emptySchema schemaDecls)
      else do
        lastKnownAst <- loadFileAst lastKnownSeason

        let lastDecls = moduleDataDecls lastKnownAst

        pure (seasonFile, diff lastDecls schemaDecls)


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
        status_ <- gitStatus file
        pure (file, status_)

  -- We only need to check our last two seasons, as only one may not yet be known (committed)
  statuses <- mapM toTuple (take 2 seasons)

  case filter (\(_, st) -> st == Committed) statuses of
    [] ->
      pure Nothing -- No known seasons
    (filePath, _):_ -> pure (Just filePath) -- Latest known commited season


checkExistingSeason :: IO (Turtle.FilePath, EvergreenStatus)
checkExistingSeason = do
  seasons <- seasonFiles

  case seasons of
    [] ->
      pure ("", Uninitiated) -- No seasons exist
    seasonFile:_ -> do
      status_ <- gitStatus seasonFile

      pure (seasonFile, status_)

      -- case status of
        -- Seasons exist
        -- status_ -> pure (seasonFile, status_) -- Latest season status
        -- Committed -> pure (seasonFile, Committed) -- But the latest one is commited
        -- _ -> case xs of -- @TODO what's going on here? Why do we need to check the prior file again...?
        --   [] -> pure (seasonFile, ChangesPending)
        --   y:ys -> do
        --     status' <- gitStatus y
        --     pure (seasonFile, status')



gitStatus :: Turtle.FilePath -> IO EvergreenStatus
gitStatus filepath = do

  let p = asText filepath

  -- print $ "Gitstatus for..." <> p
  gsPorcelain <- shellExec $ "git status --porcelain " <> p
  -- print $ firstTwo gsPorcelain
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
firstTwo text_ = (first, second)
 where
  part   = snd <$> T.uncons text_
  first  = fromMaybe '_' $ fst <$> T.uncons text_
  second = fromMaybe '_' $ fst <$> (T.uncons =<< part)


data EvergreenStatus
  = Uninitiated
  | ChangesPending
  | Committed
  | Staged
  | Deleted
  | UnexpectedEvergreenStatus
  deriving (Show, Eq)


loadSchemaAst :: String -> IO Module
loadSchemaAst modelVersion = fromParseResult <$> parseFile ("types/" ++ modelVersion ++ ".hs")


writeSeasonAst :: Turtle.FilePath -> Module -> IO ()
writeSeasonAst fileName_ ast = writeTextFile fileName_ $ T.pack $ prettyPrint ast

-- writeTextFile seasonFile $ T.pack $ prettyPrint $ astModel "Model_A" []


diff :: [Decl] -> [Decl] -> [RecordChanges]
diff d1 d2 = fmap declChanges (Dict.toList $ pairDecls d1 d2)


declChanges :: (String, (Decl, Decl)) -> RecordChanges
declChanges (recordName, (d1, d2)) =
  let f1           = fieldDecs d1
      f2           = fieldDecs d2
      added        = Added <$> (f2 \\ f1)
      removed      = Removed <$> (f1 \\ f2)
      recordStatus = if dataDeclName d1 == "Empty" then Created else Updated
  in  (recordName, recordStatus, removed ++ added)


fieldDecs :: Decl -> [Field]
fieldDecs (DataDecl _ _ _ (QualConDecl Nothing Nothing (RecDecl (Ident _) fieldDecls):_) _) =
  fmap fieldDecltoField fieldDecls
fieldDecs _ = [("Error:fieldDecs", "Field is not a DataDecl with QualConDecl", False)]


fieldDecltoField :: FieldDecl -> Field
fieldDecltoField (FieldDecl (Ident fieldName:_) (TyCon (UnQual (Ident tipe)))) = (fieldName, tipe, False)
fieldDecltoField (FieldDecl (Ident fieldName:_) (TyApp (TyCon (UnQual (Ident "Maybe"))) (TyCon (UnQual (Ident tipe)))))
  = (fieldName, "Maybe " ++ tipe, False)
fieldDecltoField _ = ("Error:fieldDecltoField", "Field doens't match shape", False)


areDataDecls :: Decl -> Decl -> Bool
areDataDecls DataDecl{} DataDecl{} = True
areDataDecls _          _          = False


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


-- TESTS

testSample :: IO ()
testSample = prettyPrint . fromParseResult <$> parseFile "evergreen/seasons/Migrations.hs" >>= putStrLn


testAst :: IO ()
testAst = putStrLn $ prettyPrint $ astModel "Schema" "ModelA" []


testParse :: IO (ParseResult Module)
testParse = parseFile "evergreen/seasons/Schema_20170812141450_be119f5af8585265f8a03acda4d86dfe6eaecb22.hs"


testWriteAst :: IO ()
testWriteAst = do
  ast <- parseFile "types/Schema.hs"
  writeTextFile "formattedAst.hs" $ T.pack $ show ast
  stdout $ inshell "hindent --style gibiansky formattedAst.hs" empty
  pure ()


writeMigrationsSummaryFile :: IO ()
writeMigrationsSummaryFile = do
  allSeasonFiles <- seasonFiles
  let seasonFilePaths = fmap asText allSeasonFiles
      ast             = migrationAst seasonFilePaths
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
