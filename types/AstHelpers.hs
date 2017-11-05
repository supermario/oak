module AstHelpers where

import Language.Haskell.Exts.Simple
import qualified Data.Map.Strict as Dict
import AstMigration (Field)


{- Given two lists of data declaration ASTs, zips them together into a Dict keyed by record name

   Useful for comparing data structures in two seperate
-}
pairDecls :: [Decl] -> [Decl] -> Dict.Map String (Decl, Decl)
pairDecls list1 list2 =
  let
    empty    = head $ moduleDataDecls $ astModel "Schema" "Empty" []

    baseDict = Dict.empty :: Dict.Map String (Decl, Decl)

    addLeft  = foldl (\dict item -> Dict.insert (dataDeclName item) (item, empty) dict) baseDict list1

    addRight = foldl (\dict item -> Dict.insertWith mergeRight (dataDeclName item) (empty, item) dict) addLeft list2

    mergeRight (c, d) (a, b) = (a, d)
  in
    addRight


moduleDataDecls :: Module -> [Decl]
moduleDataDecls (Module (Just (ModuleHead (ModuleName name) Nothing Nothing)) _ _ items) =
  let dataDeclFilter item = case item of
        DataDecl dataOrNew mContext declHead qualConDecls mDeriving -> True
        _ -> False
  in  filter dataDeclFilter items -- @TODO Should we alert the user we're skipping things in the Schema file?


dataDeclName :: Decl -> String
dataDeclName (DataDecl _ _ _ (QualConDecl Nothing Nothing (RecDecl (Ident name) _):xs) _) = name
dataDeclName d = "Error: Decl is not a DataDecl type: " ++ show d


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
