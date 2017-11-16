module AstMigration where

-- Ast functions related to creating the seasons/Schema_* migration files

import Data.Monoid ((<>))
import Language.Haskell.Exts.Simple
import ShellHelpers
import Turtle
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Text.PrettyPrint.ANSI.Leijen as A

import Debug.Trace
import AstHelpers (SeasonChanges, EvergreenRecordStatus(..), Diff(..), RecordChanges)


writeSeasonAst :: Turtle.FilePath -> Module -> IO ()
writeSeasonAst fileName_ ast = writeTextFile fileName_ $ T.pack $ prettyPrint ast


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
        { importModule    = ModuleName "Evergreen"
        , importQualified = False
        , importSrc       = False
        , importSafe      = False
        , importPkg       = Nothing
        , importAs        = Nothing
        , importSpecs     = Nothing
        }
    ]
addMigrations _ _ = undefined


migrationsAst :: SeasonChanges -> [Decl]
migrationsAst (seasonPath, recordChanges) =

  let fieldMigrations (recordName, recordStatus, changes) =
        tableCreate recordName recordStatus ++ fmap (migrationStmt recordName) changes

      migrations = recordChanges |> fmap fieldMigrations |> Prelude.concat

      tableCreate recordName recordStatus = case recordStatus of
        Created -> [tableCreateStmt recordName]
        Updated -> []
  in  [ TypeSig
        [Ident "migration"]
        ( TyFun (TyCon (UnQual (Ident "PostgresHandle")))
                (TyApp (TyCon (UnQual (Ident "IO"))) (TyCon (Special UnitCon)))
        )
      , FunBind [Match (Ident "migration") [PVar (Ident "db")] (UnGuardedRhs (Do migrations)) Nothing]
      ]


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


tableCreateStmt :: String -> Stmt
tableCreateStmt recordName = Qualifier
  (App (App (Var (UnQual (Ident "createTable"))) (Var (UnQual (Ident "db")))) (Lit (String (lowercase recordName))))



sqlType :: String -> String -> String
sqlType name tipe = case tipe of

  "PrimaryInt"     -> "serial primary key"

  "String"         -> "varchar(255) not null"
  "Maybe String"   -> "varchar(255)"

  "Int"            -> "int8 not null"
  "Maybe Int"      -> "int8"

  "Datetime"       -> "timestamp with time zone not null"
  "Maybe Datetime" -> "timestamp with time zone"

  _ ->

    -- @TODO NEXT so here we likely need to take an additional value that represents the AST we have,
    -- so that we can query for references between objects. Need to look up the postgresql syntax for
    -- foreign key references, see if that matches what we want here.
    -- Probably also need to take a look at how Rails does things, and what SQL it creates/writes to DB.
    trace ("sqlType tried to process unknown type: " ++ name ++ " :: " ++ tipe) "ERROR UNKNOWN TYPE"


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
