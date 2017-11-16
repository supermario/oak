module AstHelpers where

import Language.Haskell.Exts.Simple
import qualified Data.Map.Strict as Dict
import Turtle
import Data.List ((\\))


-- @TODO make this typesafe
-- (name, type, nullable)
type Field = (String, String, Bool)


data Diff
  = Added Field
  | Removed Field
  deriving (Show)


-- Name of record, record status, field changesets
type RecordChanges = Dict.Map String (EvergreenRecordStatus, [Diff])


data EvergreenRecordStatus
  = Created
  | Updated
  deriving (Show, Eq)


-- Path to migration, list of record changesets
type SeasonChanges = (Turtle.FilePath, RecordChanges)


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


diff :: [Decl] -> [Decl] -> RecordChanges
diff d1 d2 = Dict.fromList $ fmap declChanges (Dict.toList $ pairDecls d1 d2)


declChanges :: (String, (Decl, Decl)) -> (String, (EvergreenRecordStatus, [Diff]))
declChanges (recordName, (d1, d2)) =
  let f1           = fieldDecs d1
      f2           = fieldDecs d2
      added        = Added <$> (f2 \\ f1)
      removed      = Removed <$> (f1 \\ f2)
      recordStatus = if dataDeclName d1 == "Empty" then Created else Updated
  in  (recordName, (recordStatus, removed ++ added))


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


moduleDataDecls :: Module -> [Decl]
moduleDataDecls (Module (Just (ModuleHead (ModuleName name) Nothing Nothing)) _ _ items) =
  let dataDeclFilter item = case item of
        DataDecl dataOrNew mContext declHead qualConDecls mDeriving -> True
        _ -> False
  in  filter dataDeclFilter items -- @TODO Should we alert the user we're skipping things in the Schema file?
moduleDataDecls _ = undefined


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
