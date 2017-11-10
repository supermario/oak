module AstSchema where

{-

Ast functions related to Schema.hs

-}

import Language.Haskell.Exts.Simple
import Data.Text
import ShellHelpers


loadSchemaAst :: String -> IO Module
loadSchemaAst modelVersion = fromParseResult <$> parseFile ("types/" ++ modelVersion ++ ".hs")


schemaSha :: Module -> Text
schemaSha = tShow . sha1 . tShow
