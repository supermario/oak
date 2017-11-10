{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module AstMigrations where

{-

Ast functions related to creating the Migrations.hs summary file

-}

import Data.Text
import Language.Haskell.Exts.Simple
import Safe (lastMay, atMay)
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)
import ShellHelpers
import Turtle


writeMigrationsSummaryFile :: IO ()
writeMigrationsSummaryFile = do
  allSeasonFiles <- seasonFiles
  let seasonFilePaths = fmap asText allSeasonFiles
      ast             = migrationAst seasonFilePaths
  writeTextFile "evergreen/Migrations.hs" $ pack $ prettyPrint ast
  pure ()


resetMigrationsSummaryFile :: IO ()
resetMigrationsSummaryFile = do
  let ast = migrationAst []
  writeTextFile "evergreen/Migrations.hs" $ pack $ prettyPrint ast
  pure ()


migrationAst :: [Text] -> Module
migrationAst seasonPaths = Module
  (Just (ModuleHead (ModuleName "Migrations") Nothing Nothing))
  []
  (  [ ImportDecl
       { importModule    = ModuleName "Hilt.Postgres"
       , importQualified = True
       , importSrc       = False
       , importSafe      = False
       , importPkg       = Nothing
       , importAs        = Nothing
       , importSpecs     = Nothing
       }
     , ImportDecl
       { importModule    = ModuleName "Data.Text"
       , importQualified = False
       , importSrc       = False
       , importSafe      = False
       , importPkg       = Nothing
       , importAs        = Nothing
       , importSpecs     = Just (ImportSpecList False [IAbs NoNamespace (Ident "Text")])
       }
     ]
  ++ fmap qualifiedImport seasonNames
  )
  [ TypeSig
    [Ident "allMigrations"]
    ( TyList
      ( TyTuple
        Boxed
        [ TyCon (UnQual (Ident "Text"))
        , TyFun (TyCon (Qual (ModuleName "Hilt.Postgres") (Ident "Handle")))
                (TyApp (TyCon (UnQual (Ident "IO"))) (TyCon (Special UnitCon)))
        ]
      )
    )
  , PatBind (PVar (Ident "allMigrations")) (UnGuardedRhs (List (fmap migrationTuple seasonNames))) Nothing
  ]
  where seasonNames = Prelude.reverse $ fmap getFilename seasonPaths

getFilename :: Text -> Text
getFilename file =
  fromMaybe ("ERROR: could not parse SHA from filename " <> file)
    $ flip atMay 0
    $ splitOn ("." :: Text)
    $ fromMaybe ("NOPE" :: Text)
    $ lastMay
    $ splitOn "/" file


qualifiedImport :: Text -> ImportDecl
qualifiedImport name = ImportDecl
  { importModule    = ModuleName $ unpack name
  , importQualified = True
  , importSrc       = False
  , importSafe      = False
  , importPkg       = Nothing
  , importAs        = Nothing
  , importSpecs     = Nothing
  }

migrationTuple :: Text -> Exp
migrationTuple name = Tuple Boxed
                            [Lit (String $ unpack sha), Var (Qual (ModuleName $ unpack name) (Ident "migration"))]
  where sha = fromMaybe ("ERROR: could not parse SHA from filename " <> name) (atMay (splitOn "_" name) 2)
