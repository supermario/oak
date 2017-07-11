module Oak exposing (..)

import Ast
import Ast.Expression exposing (..)
import Ast.Helpers exposing (..)
import Html exposing (..)
import Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode as JD
import Platform.Sub
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Combine exposing (ParseResult)
import Dict
import List.Extra as List


v1text : String
v1text =
  -- Version 1, Baseline
  """
type alias Model =
  { name : String
  }
"""


v2text : String
v2text =
  -- Version 2, Add field
  """
type alias Model =
  { name : String
  , lastName : String
  }
"""


v3text : String
v3text =
  -- Version 3, Remove field
  """
type alias Model =
  { firstName : String
  , lastName : String
  }

"""


v4text : String
v4text =
  -- Version 4, Change field type
  """
type alias Model =
  { firstName : Int
  , lastName : String
  }

"""


v5text : String
v5text =
  -- Version 5, Change to sub-record
  """
type alias Model =
  { firstName : { english : String, jap : String }
  , lastName : String
  }

"""


t1text : String
t1text =
  "type MyUnion = First | Second"


t2text : String
t2text =
  "type MyUnion = First | Third"


type Msg
  = Replace String String String
  | Loaded String String (Result Error String)


type alias Item =
  { moduleName : String
  , text : Maybe String
  , parsed :
      Maybe (Result (Combine.ParseErr ()) (Combine.ParseOk () (List Statement)))
  }


type alias Package =
  { package : String
  , items : List Item
  }


type alias Model =
  List Package


testFiles : List ( String, List String )
testFiles =
  []


init : ( Model, Cmd Msg )
init =
  (Package "n/a"
    ([ v1text, v2text, v3text, v4text, v5text, t1text, t2text ]
      |> List.map
          (\t ->
            Item "Custom Editor"
              (Just t)
              (Just <| Ast.parse t)
          )
    )
    :: List.map
        (\( pkg, items ) ->
          Package pkg <|
            List.map
              (\moduleName ->
                Item moduleName Nothing Nothing
              )
              items
        )
        testFiles
  )
    ! ((List.concatMap
          (\( package, files ) ->
            List.map
              (\file ->
                send (Loaded package file)
                  (getString <| "https://raw.githubusercontent.com/" ++ package ++ file ++ ".elm")
              )
              files
          )
       )
        testFiles
      )


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
  case action of
    Replace pkg name m ->
      List.map
        (\package ->
          if package.package == pkg then
            { package
              | items =
                  List.map
                    (\item ->
                      if item.moduleName == name then
                        { item
                          | text = Just m
                          , parsed = Just (Ast.parse m)
                        }
                      else
                        item
                    )
                    package.items
            }
          else
            package
        )
        model
        ! []

    Loaded pkg name result ->
      case result of
        Ok data ->
          List.map
            (\package ->
              if package.package == pkg then
                { package
                  | items =
                      List.map
                        (\item ->
                          if item.moduleName == name then
                            { item
                              | text = Just data
                              , parsed = Just (Ast.parse data)
                            }
                          else
                            item
                        )
                        package.items
                }
              else
                package
            )
            model
            ! []

        Err err ->
          let
            x =
              Debug.log "error" err
          in
            model ! []


withChild : a -> List (Html Msg) -> Html Msg
withChild title children =
  li []
    [ text <| toString title
    , ul [] children
    ]


expression : Expression -> Html Msg
expression e =
  case e of
    List es ->
      withChild e (List.map expression es)

    Application e1 e2 ->
      withChild e
        [ expression e1
        , expression e2
        ]

    e ->
      li [] [ text <| toString e ]


statement : Statement -> Html Msg
statement s =
  case s of
    FunctionDeclaration (Function _ _ e) ->
      withChild s [ expression e ]

    s ->
      div [] [ text <| toString s ]



--tree : Item -> Html Msg


tree : Result error ( a, b, List Statement ) -> Html Msg
tree ast =
  case ast of
    Ok ( _, _, statements ) ->
      div [] (List.map statement statements)

    err ->
      div
        [ style
            [ ( "margin", "10px" )
            , ( "min-height", "600px" )
            ]
        ]
        [ text <| toString err ]


countItems : Bool -> Model -> Int
countItems value model =
  model
    |> List.map .items
    |> List.concatMap identity
    |> List.filter
        (\i ->
          case i.parsed of
            Just ast ->
              case ast of
                Ok _ ->
                  value

                _ ->
                  not value

            _ ->
              False
        )
    |> List.length


view : Model -> Html Msg
view model =
  Grid.containerFluid [] <|
    [ CDN.stylesheet
      -- , navbar model
    ]
      ++ mainContent model


mainContent : List Package -> List (Html Msg)
mainContent model =
  [ Grid.simpleRow
      [ Grid.col
          [ Col.xs12 ]
          [ h1 []
              [ text "All items: "
              , text <|
                  toString
                    (model
                      |> List.map .items
                      |> List.concatMap identity
                      |> List.length
                    )
              , text " Success count: "
              , text <| toString <| countItems True model
              , text " Failure count: "
              , text <| toString <| countItems False model
              ]
          ]
      ]
  ]
    ++ (List.concatMap identity
          (List.map
            (\package ->
              [ Grid.simpleRow
                  [ Grid.col [ Col.xs12 ]
                      [ h2 [] [ text <| "Package: " ++ package.package ] ]
                  ]
              ]
                ++ (List.concatMap identity
                      (List.map
                        (\item ->
                          [ Grid.simpleRow
                              [ Grid.col [ Col.xs12 ]
                                  [ h2 [] [ text <| "Module: " ++ item.moduleName ] ]
                              ]
                          , Grid.simpleRow
                              [ Grid.col
                                  [ Col.xs4 ]
                                  [ textarea
                                      [ on "input" (JD.map (Replace package.package item.moduleName) targetValue)
                                      , style
                                          [ ( "width", "100%" )
                                          , ( "padding", "0" )
                                          , ( "position", "absolute" )
                                          , ( "top", "0" )
                                          , ( "bottom", "0" )
                                          , ( "left", "0" )
                                          , ( "right", "0" )
                                          ]
                                      ]
                                      [ text <|
                                          case item.text of
                                            Just txt ->
                                              txt

                                            _ ->
                                              ""
                                      ]
                                  ]
                              , Grid.col
                                  [ Col.xs8
                                  ]
                                  [ case item.parsed of
                                      Just ast ->
                                        tree ast

                                      _ ->
                                        text ""
                                  ]
                              ]
                          ]
                        )
                        package.items
                      )
                   )
            )
            model
          )
       )


main : Program Never Model Msg
main =
  Html.program
    { init = init
    , update = update
    , subscriptions = \m -> Sub.none
    , view = view
    }


v0 : Statement
v0 =
  -- Identical to v0 for testing
  TypeAliasDeclaration
    (TypeConstructor [ "Model" ] [])
    (TypeRecord
      ([ ( "name", TypeConstructor [ "String" ] [] )
       ]
      )
    )


v1 : Statement
v1 =
  -- Initial model
  TypeAliasDeclaration
    (TypeConstructor [ "Model" ] [])
    (TypeRecord
      ([ ( "name", TypeConstructor [ "String" ] [] )
       ]
      )
    )


v2 : Statement
v2 =
  -- New field lastName
  TypeAliasDeclaration
    (TypeConstructor [ "Model" ] [])
    (TypeRecord
      ([ ( "name", TypeConstructor [ "String" ] [] )
       , ( "lastName", TypeConstructor [ "String" ] [] )
       ]
      )
    )


v3 : Statement
v3 =
  -- Changed field name name -> firstName
  TypeAliasDeclaration
    (TypeConstructor [ "Model" ] [])
    (TypeRecord
      ([ ( "firstName", TypeConstructor [ "String" ] [] )
       , ( "lastName", TypeConstructor [ "String" ] [] )
       ]
      )
    )


v4 : Statement
v4 =
  -- Changed type
  TypeAliasDeclaration
    (TypeConstructor [ "Model" ] [])
    (TypeRecord
      ([ ( "firstName", TypeConstructor [ "Int" ] [] )
       , ( "lastName", TypeConstructor [ "String" ] [] )
       ]
      )
    )


v5 : Statement
v5 =
  TypeAliasDeclaration
    -- Mega changed type
    (TypeConstructor [ "Model" ] [])
    (TypeRecord
      ([ ( "firstName"
         , TypeRecord
            ([ ( "english", TypeConstructor [ "String" ] [] )
             , ( "jap", TypeConstructor [ "String" ] [] )
             ]
            )
         )
       , ( "lastName", TypeConstructor [ "String" ] [] )
       ]
      )
    )


t1 : Statement
t1 =
  TypeDeclaration (TypeConstructor [ "MyUnion" ] []) ([ TypeConstructor [ "First" ] [], TypeConstructor [ "Second" ] [] ])


t2 : Statement
t2 =
  TypeDeclaration (TypeConstructor [ "MyUnion" ] []) ([ TypeConstructor [ "First" ] [], TypeConstructor [ "Third" ] [] ])


typeDiff : Type -> Type -> Result String String
typeDiff t1 t2 =
  if t1 == t2 then
    Ok "Types are identical"
  else
    case sameToplevelType t1 t2 of
      Just t ->
        case ( t1, t2 ) of
          ( TypeRecord f1, TypeRecord f2 ) ->
            let
              diff =
                fieldsDiff f1 f2
            in
              Err <| "Record type differs:" ++ (toString diff)

          ( TypeConstructor qt1 tl1, TypeConstructor qt2 tl2 ) ->
            if qt1 /= qt2 then
              Err <| "Type constructor differs: " ++ toString qt1 ++ " vs " ++ toString qt2
            else
              typeDiffList tl1 tl2

          ( a, _ ) ->
            Err <| "Unimplemented comparison: " ++ unionTag a

      -- TypeConstructor QualifiedType (List Type) ->
      -- TypeVariable Name ->
      -- TypeRecordConstructor Type (List (Name, Type)) ->
      -- TypeRecord (List (Name, Type)) ->
      -- TypeTuple (List Type) ->
      -- TypeApplication Type Type ->
      -- NamedType Type Name ->
      Nothing ->
        -- @TODO represent a type difference properly
        Err <| "Record types differ: " ++ unionTag t1 ++ " vs " ++ unionTag t2


typeDiffList : List Type -> List Type -> Result String String
typeDiffList tl1 tl2 =
  List.map2 typeDiff tl1 tl2
    |> List.find
        (\x ->
          case x of
            Err _ ->
              True

            Ok _ ->
              False
        )
    |> Maybe.withDefault (Ok "Types are identical")


sameToplevelType : a -> b -> Maybe String
sameToplevelType t1 t2 =
  if unionTag t1 == unionTag t2 then
    Just <| unionTag t1
  else
    Nothing


unionTag : a -> String
unionTag t =
  toString t |> String.split " " |> List.head |> Maybe.withDefault ""



-- | TypeAliasDeclaration Type Type
-- | TypeDeclaration Type (List Type)
-- TypeDeclaration c t ->
--   ( c, t )


statementDiff : Statement -> Statement -> Result String String
statementDiff s1 s2 =
  let
    d1 =
      isTypeDeclaration s1

    d2 =
      isTypeDeclaration s2
  in
    if unionTag s1 /= unionTag s2 then
      Err <| "Cannot compare non-matching union tags: " ++ unionTag s1 ++ " and " ++ unionTag s2
    else if s1 == s2 then
      Ok "Records types are identical"
    else
      case ( d1, d2 ) of
        ( Ok (TypeAliasDeclaration c1 t1), Ok (TypeAliasDeclaration c2 t2) ) ->
          if c1 == c2 then
            typeDiff t1 t2
          else
            Err <| "Constructors are different: " ++ toString c1 ++ " vs " ++ toString c2

        ( Ok (TypeDeclaration c1 tl1), Ok (TypeDeclaration c2 tl2) ) ->
          if c1 == c2 then
            typeDiffList tl1 tl2
          else
            Err <| "Constructors are different: " ++ toString c1 ++ " vs " ++ toString c2

        ( a, b ) ->
          Err <| "Unsupported diff types: " ++ toString a ++ ", " ++ toString b


isTypeDeclaration : Statement -> Result String Statement
isTypeDeclaration s =
  case s of
    TypeAliasDeclaration constructor typ ->
      Ok s

    TypeDeclaration constructor types ->
      Ok s

    ModuleDeclaration _ _ ->
      Err "ModuleDeclaration is not a comparable type"

    EffectsModuleDeclaration _ _ _ ->
      Err "EffectsModuleDeclaration is not a comparable type"

    PortModuleDeclaration _ _ ->
      Err "PortModuleDeclaration is not a comparable type"

    ImportStatement _ _ _ ->
      Err "ImportStatement is not a comparable type"

    PortTypeDeclaration _ _ ->
      Err "PortTypeDeclaration is not a comparable type"

    PortDeclaration _ _ _ ->
      Err "PortDeclaration is not a comparable type"

    FunctionTypeDeclaration _ _ ->
      Err "FunctionTypeDeclaration is not a comparable type"

    FunctionDeclaration _ ->
      Err "FunctionDeclaration is not a comparable type"

    InfixDeclaration _ _ _ ->
      Err "InfixDeclaration is not a comparable type"

    Comment _ ->
      Err "Comment is not a comparable type"


type Changes a
  = Added a
  | Removed a
  | Changed a


fieldsDiff : List ( Name, Type ) -> List ( Name, Type ) -> List (Changes ( Name, Type ))
fieldsDiff list1 list2 =
  let
    d1 =
      list1 |> List.map toComparable |> Dict.fromList

    d2 =
      list2 |> List.map toComparable |> Dict.fromList

    removed =
      Dict.diff d1 d2 |> Dict.toList |> List.map (Tuple.second >> Removed)

    added =
      Dict.diff d2 d1 |> Dict.toList |> List.map (Tuple.second >> Added)
  in
    removed ++ added


toComparable : ( Name, Type ) -> ( String, ( Name, Type ) )
toComparable ( n, t ) =
  ( toString n ++ toString t, ( n, t ) )


test : Result String String
test =
  statementDiff v4 v5
