module AstMigration where

{-

Ast functions related to creating the seasons/Schema_* migration files

-}

import Data.Monoid ((<>))
import Data.Text
import Language.Haskell.Exts.Simple
import Safe (lastMay, atMay)
import ShellHelpers
import Turtle


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

  let migrationsFor (recordName, recordStatus, changes) = fmap (migrationStmt recordName) changes

      migrations = recordChanges |> fmap migrationsFor |> Prelude.concat
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


-- @TODO handle nullable
sqlType :: String -> String -> String
sqlType name tipe = case name of
  "id" -> "serial primary key"
  _    -> case tipe of
    "String" -> "varchar(255) not null"
    _        -> "ERROR UNKNOWN TYPE"




-- Display Helpers

toSeasonDiffText :: Diff -> String
toSeasonDiffText change = case change of
    -- @TODO respect bool
  Added   (name, tipe, bool) -> "        added:   " <> name <> " :: " <> tipe
  Removed (name, tipe, bool) -> "        removed: " <> name <> " :: " <> tipe
