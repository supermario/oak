module AstMigration where

-- Ast functions related to creating the seasons/Schema_* migration files

import Data.Monoid ((<>))
import Language.Haskell.Exts.Simple
import Safe (lastMay, atMay)
import ShellHelpers
import Turtle
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Text.PrettyPrint.ANSI.Leijen as A

import Debug.Trace


-- @TODO make this typesafe
-- (name, type, nullable)
type Field = (String, String, Bool)


data Diff
  = Added Field
  | Removed Field
  | Debug String
  deriving (Show)


data EvergreenRecordStatus
  = Created
  | Updated
  deriving (Show, Eq)


-- Name of record, record status, field changesets
type RecordChanges = (String, EvergreenRecordStatus, [Diff])

-- Path to migration, list of record changesets
type SeasonChanges = (Turtle.FilePath, [RecordChanges])


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


migrationsAst :: SeasonChanges -> [Decl]
migrationsAst (seasonPath, recordChanges) =

  let fieldMigrations (recordName, recordStatus, changes) =
        tableCreate recordName recordStatus ++ fmap (migrationStmt recordName) changes

      migrations = recordChanges |> fmap fieldMigrations |> Prelude.concat

      tableCreate recordName recordStatus = case recordStatus of
        Created -> [tableCreateStmt recordName]
        Updated -> []
  in  [FunBind [Match (Ident "migration") [PVar (Ident "db")] (UnGuardedRhs (Do migrations)) Nothing]]


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


tableCreateStmt :: String -> Stmt
tableCreateStmt recordName = Qualifier
  (App (App (Var (UnQual (Ident "createTable"))) (Var (UnQual (Ident "db")))) (Lit (String (lowercase recordName))))


-- @TODO handle nullable
sqlType :: String -> String -> String
sqlType name tipe = case name of
  "id" -> "serial primary key"
  _    -> case tipe of

    "String"       -> "varchar(255) not null"
    "Maybe String" -> "varchar(255)"

    "Int"          -> "int8 not null"
    "Maybe Int"    -> "int8"

    _              -> trace ("sqlType tried to process unknown type: " ++ tipe ++ " on " ++ name) "ERROR UNKNOWN TYPE"


-- Display Helpers

showSeasonChanges :: Turtle.FilePath -> SeasonChanges -> IO ()
showSeasonChanges seasonFile (filepath, recordChanges) = do
  T.putStrLn ""
  T.putStrLn ""
  T.putStrLn "Schema changes to be committed:"
  T.putStrLn $ "  (season remembered in " <> asText seasonFile <> ")"
  T.putStrLn ""
  mapM_ showRecordChanges recordChanges
  T.putStrLn ""


showRecordChanges :: RecordChanges -> IO ()
showRecordChanges (recordName, recordStatus, changes) = if not $ null changes
  then do
    putColored $ "  " <> blue recordName <> " record will be " <> A.text (lowercase $ show recordStatus)
    T.putStrLn ""
    T.putStrLn ""
    mapM_ (putColoredLn . toSeasonDiffText) changes
    T.putStrLn ""
  else pure ()


toSeasonDiffText :: Diff -> A.Doc
toSeasonDiffText change = case change of
    -- @TODO respect bool
  Added   (name, tipe, bool) -> green $ "        added:   " <> name <> " :: " <> tipe
  Removed (name, tipe, bool) -> red $ "        removed: " <> name <> " :: " <> tipe
