{-# LANGUAGE QuasiQuotes #-}

module ParserSpec
  ( spec,
  )
where

import qualified AST
import Data.Bool (Bool (..))
import Data.Function (($))
import qualified Data.Map.Strict as Map
import Data.Maybe (Maybe (..))
import Parser
import Test.Hspec
  ( Spec,
    context,
    describe,
    fcontext,
    fit,
    it,
    shouldBe,
  )
import Test.Hspec.Megaparsec
  ( failsLeaving,
    initialState,
    shouldFailOn,
    shouldParse,
    shouldSucceedOn,
    succeedsLeaving,
  )
import Text.Megaparsec
  ( ParseErrorBundle,
    parse,
    runParser',
  )
import Text.RawString.QQ (r)

spec :: Spec
spec = describe "Parser" $ do
  context "executable definition" $ do
    context "values" $ do
      it "parses Variable Value" $ do
        parse value "" "$input" `shouldParse` (AST.VVariable $ AST.Variable "input")
      it "parses Int Value" $ do
        parse value "" "4" `shouldParse` (AST.VInt 4)
      it "parses Float Value" $ do
        parse value "" "4.2" `shouldParse` (AST.VFloat 4.2)
      it "parses String Value" $ do
        parse value "" [r|"Bad Girl RiRi\n"|] `shouldParse` (AST.VString "Bad Girl RiRi\n")
      it "parses Boolean Value" $ do
        parse value "" "false" `shouldParse` (AST.VBool False)
        parse value "" "true" `shouldParse` (AST.VBool True)
      it "parses Null Value" $ do
        parse value "" "null" `shouldParse` (AST.VNull)
      context "enum value" $ do
        it "should throw on 'true', 'false' and 'null'" $ do
          parse enumValue "" `shouldFailOn` "true "
          parse enumValue "" `shouldFailOn` "false "
          parse enumValue "" `shouldFailOn` "null "
        it "parses" $ do
          parse value "" "EAST " `shouldParse` (AST.VEnum $ AST.EnumValue "EAST")
      it "parses List Value" $ do
        parse value "" "[100, $gec, \"gec\"]"
          `shouldParse` ( AST.VList
                            [ AST.VInt 100,
                              AST.VVariable $ AST.Variable "gec",
                              AST.VString "gec"
                            ]
                        )
      it "parses Object Value" $ do
        parse value "" "{ gec: \"gec\", goc: 100, gac: true, gic: $gec }"
          `shouldParse` ( AST.VObject $
                            Map.fromList $
                              [ ("gec", AST.VString "gec"),
                                ("goc", AST.VInt 100),
                                ("gac", AST.VBool True),
                                ("gic", AST.VVariable (AST.Variable "gec"))
                              ]
                        )
    context "const values" $ do
      it "parses Int Value Const" $ do
        parse valueConst "" "4" `shouldParse` (AST.VCInt 4)
      it "parses Float Value Const" $ do
        parse valueConst "" "4.2" `shouldParse` (AST.VCFloat 4.2)
      it "parses String Value Const" $ do
        parse valueConst "" [r|"Bad Girl RiRi\n"|] `shouldParse` (AST.VCString "Bad Girl RiRi\n")
      it "parses Boolean Value Const" $ do
        parse valueConst "" "false" `shouldParse` (AST.VCBool False)
        parse valueConst "" "true" `shouldParse` (AST.VCBool True)
      it "parses Null Value Const" $ do
        parse valueConst "" "null" `shouldParse` (AST.VCNull)
      it "parses List Value Const" $ do
        parse valueConst "" "[100, true, \"gec\"]"
          `shouldParse` ( AST.VCList
                            [ AST.VCInt 100,
                              AST.VCBool True,
                              AST.VCString "gec"
                            ]
                        )
      it "parses Object Value Const" $ do
        parse
          valueConst
          ""
          "{ gec: \"gec\", goc: 100, gac: true, gic: null }"
          `shouldParse` ( AST.VCObject $
                            Map.fromList $
                              [ ("gec", AST.VCString "gec"),
                                ("goc", AST.VCInt 100),
                                ("gac", AST.VCBool True),
                                ("gic", AST.VCNull)
                              ]
                        )
    context "types" $ do
      it "parses named type" $ do
        parse gQLType "" "Int"
          `shouldParse` (AST.GQLNamedType $ AST.NamedType "Int")
      it "parses list type" $ do
        parse gQLType "" "[Int]"
          `shouldParse` (AST.ListType $ AST.GQLNamedType $ AST.NamedType "Int")
      it "parses no-null type" $ do
        parse gQLType "" "Int!"
          `shouldParse` (AST.NonNullType $ AST.NonNullNamedType "Int")
      it "parses no-null list type" $ do
        parse gQLType "" "[Int]!"
          `shouldParse` ( AST.NonNullType $
                            AST.NonNullListType $
                              AST.GQLNamedType $
                                AST.NamedType "Int"
                        )
      it "parses no-null list type with non-null named type inside" $ do
        parse gQLType "" "[Int!]!"
          `shouldParse` ( AST.NonNullType $
                            AST.NonNullListType $
                              AST.NonNullType $
                                AST.NonNullNamedType "Int"
                        )
    context "field" $ do
      it "parses field with alias" $ do
        parse
          field
          ""
          "smallPic: profilePic(size: 64)"
          `shouldParse` AST.Field
            (AST.Name "profilePic")
            [ AST.Argument
                (AST.Name "size")
                (AST.VInt 64)
            ]
            []
            (Just "smallPic")
      it "parses field without alias" $ do
        parse
          field
          ""
          "profilePic(size: 64)"
          `shouldParse` AST.Field
            (AST.Name "profilePic")
            [ AST.Argument
                (AST.Name "size")
                (AST.VInt 64)
            ]
            []
            Nothing
    context "directive" $ do
      it "parses without arguments" $ do
        parse
          directives
          ""
          [r|@super @power|]
          `shouldParse` [ AST.Directive (AST.Name "super") [],
                          AST.Directive (AST.Name "power") []
                        ]
      it "parses with arguments" $ do
        parse
          directives
          ""
          [r|@addExternalFields(source: "profiles") @excludeField(name: "photo")|]
          `shouldParse` [ AST.Directive (AST.Name "addExternalFields") [AST.Argument "source" (AST.VString "profiles")],
                          AST.Directive (AST.Name "excludeField") [AST.Argument "name" (AST.VString "photo")]
                        ]

    context "fragments" $ do
      it "parses definition" $ do
        parse
          fragment
          ""
          [r|fragment friendFields on User {
              id
              name
              profilePic(size: 50)
            }|]
          `shouldParse` ( AST.FragmentDefinition
                            (AST.FragmentName "friendFields")
                            (AST.TypeCondition (AST.NamedType "User"))
                            []
                            [ AST.SelectionField $ AST.Field "id" [] [] Nothing,
                              AST.SelectionField $ AST.Field "name" [] [] Nothing,
                              AST.SelectionField $
                                AST.Field
                                  "profilePic"
                                  [AST.Argument "size" (AST.VInt 50)]
                                  []
                                  Nothing
                            ]
                        )
      it "throws if fragment name is on" $ do
        parse
          fragmentName
          ""
          `shouldFailOn` "on  "
        parse
          fragmentName
          ""
          `shouldFailOn` "on  e"
        parse
          fragmentName
          ""
          `shouldSucceedOn` "oneeeeeee"
        parse
          fragmentName
          ""
          `shouldSucceedOn` "name"

      it "parses fragment spread" $ do
        parse
          fragmentSpread
          ""
          "...friendFields"
          `shouldParse` ( AST.FragmentSpread
                            (AST.FragmentName "friendFields")
                            []
                        )
    context "selectionSet" $ do
      it "parses fragment spreads" $
        parse
          selectionSet
          ""
          [r|{
            friends {
              ...friendFields
            }
            ...profilePhoto
          }
        |]
          `shouldParse` [ AST.SelectionField $
                            AST.Field
                              (AST.Name "friends")
                              []
                              [ AST.SelectionFragmentSpread $
                                  AST.FragmentSpread (AST.FragmentName "friendFields") []
                              ]
                              Nothing,
                          AST.SelectionFragmentSpread $
                            AST.FragmentSpread (AST.FragmentName "profilePhoto") []
                        ]
      it "parses inline fragments without type condition" $
        parse
          selectionSet
          ""
          [r|{
          ... @include(if: $expandedInfo) {
            firstName
            lastName
            birthday
          }
        }
        |]
          `shouldParse` [ AST.SelectionInlineFragment $
                            AST.InlineFragment
                              Nothing
                              [ AST.Directive
                                  (AST.Name "include")
                                  [ AST.Argument
                                      (AST.Name "if")
                                      (AST.VVariable $ AST.Variable "expandedInfo")
                                  ]
                              ]
                              [ AST.SelectionField $ AST.Field (AST.Name "firstName") [] [] Nothing,
                                AST.SelectionField $ AST.Field (AST.Name "lastName") [] [] Nothing,
                                AST.SelectionField $ AST.Field (AST.Name "birthday") [] [] Nothing
                              ]
                        ]
      it "parses inline fragments with type condition" $
        parse
          selectionSet
          ""
          [r|{
          ... on User {
            firstName
            lastName
            birthday
          }
        }|]
          `shouldParse` [ AST.SelectionInlineFragment $
                            AST.InlineFragment
                              (Just $ AST.TypeCondition $ AST.NamedType "User")
                              []
                              [ AST.SelectionField $ AST.Field (AST.Name "firstName") [] [] Nothing,
                                AST.SelectionField $ AST.Field (AST.Name "lastName") [] [] Nothing,
                                AST.SelectionField $ AST.Field (AST.Name "birthday") [] [] Nothing
                              ]
                        ]
    context "document" $ do
      it "parses operation with name" $ do
        parse
          document
          ""
          [r|mutation like { 
          like
         } |]
          `shouldParse` ( AST.DocumentOperation $
                            AST.Operation AST.Mutation (Just "like") [] [] $
                              [AST.SelectionField $ AST.Field "like" [] [] Nothing]
                        )
      it "parses operation with arguments" $ do
        parse
          document
          ""
          [r|query like($id: Int) { 
          likeStory (id: $id)
         } |]
          `shouldParse` ( AST.DocumentOperation $
                            AST.Operation
                              AST.Query
                              (Just "like")
                              [ AST.VariableDefinition
                                  (AST.Variable "id")
                                  (AST.GQLNamedType $ AST.NamedType "Int")
                              ]
                              []
                              $ [ AST.SelectionField $
                                    AST.Field
                                      "likeStory"
                                      [ AST.Argument
                                          (AST.Name "id")
                                          (AST.VVariable (AST.Variable "id"))
                                      ]
                                      []
                                      Nothing
                                ]
                        )
      it "parses operation with directives" $ do
        parse
          document
          ""
          [r|query like($id: Int) @excludeField(name: "photo") @addExternalFields(source: "profiles") { 
          likeStory (id: $id)
          } |]
          `shouldParse` ( AST.DocumentOperation $
                            AST.Operation
                              AST.Query
                              (Just "like")
                              [ AST.VariableDefinition
                                  (AST.Variable "id")
                                  (AST.GQLNamedType $ AST.NamedType "Int")
                              ]
                              [ AST.Directive
                                  (AST.Name "excludeField")
                                  [AST.Argument "name" (AST.VString "photo")],
                                AST.Directive (AST.Name "addExternalFields") [AST.Argument "source" (AST.VString "profiles")]
                              ]
                              $ [ AST.SelectionField $
                                    AST.Field
                                      "likeStory"
                                      [ AST.Argument
                                          (AST.Name "id")
                                          (AST.VVariable (AST.Variable "id"))
                                      ]
                                      []
                                      Nothing
                                ]
                        )
      it "parses top level fragment definition" $ do
        parse
          document
          ""
          [r|fragment friendFields on User {
          id
          name
          profilePic(size: 50)
        } |]
          `shouldParse` ( AST.DocumentFragment $
                            AST.FragmentDefinition
                              (AST.FragmentName "friendFields")
                              (AST.TypeCondition (AST.NamedType "User"))
                              []
                              [ AST.SelectionField $ AST.Field "id" [] [] Nothing,
                                AST.SelectionField $ AST.Field "name" [] [] Nothing,
                                AST.SelectionField $
                                  AST.Field
                                    "profilePic"
                                    [AST.Argument "size" (AST.VInt 50)]
                                    []
                                    Nothing
                              ]
                        )
  context "type system definition" $ do
    context "schema definition" $ do
      it "parses schema definition with description" $ do
        parse
          schemaDefinition
          ""
          [r|"""Describe"""
        schema {
          query: Query
          mutation: Mutation
          subscription: Subscription 
        }|]
          `shouldParse` AST.SchemaDefinition
            (Just $ AST.Description "Describe")
            []
            [ AST.RootOperationTypeDefinition AST.Query $ AST.NamedType "Query",
              AST.RootOperationTypeDefinition AST.Mutation $ AST.NamedType "Mutation",
              AST.RootOperationTypeDefinition AST.Subscription $ AST.NamedType "Subscription"
            ]
      it "parses schema without description" $ do
        parse
          schemaDefinition
          ""
          [r|schema {
          query: MyQueryRootType
          mutation: MyMutationRootType
          subscription: MySubscriptionRootType 
        }|]
          `shouldParse` AST.SchemaDefinition
            Nothing
            []
            [ AST.RootOperationTypeDefinition AST.Query $ AST.NamedType "MyQueryRootType",
              AST.RootOperationTypeDefinition AST.Mutation $ AST.NamedType "MyMutationRootType",
              AST.RootOperationTypeDefinition AST.Subscription $ AST.NamedType "MySubscriptionRootType"
            ]
    context "type definitions" $ do
      it "parses scalar type definition" $ do
        parse
          typeDefinition
          ""
          "\"Description\" scalar URL "
          `shouldParse` AST.TypeDefinitionScalar
            ( AST.ScalarTypeDefinition
                (Just $ AST.Description "Description")
                (AST.Name "URL")
                []
            )
      context "interface type definition" $ do
        it "parses without implements clause" $ do
          parse
            typeDefinition
            ""
            [r|"describe"
              interface NamedEntity {
                   name: String
              }
          |]
            `shouldParse` AST.TypeDefinitionInterface
              ( AST.InterfaceTypeDefinition
                  (Just $ AST.Description "describe")
                  (AST.Name "NamedEntity")
                  []
                  []
                  [ AST.FieldDefinition
                      Nothing
                      (AST.Name "name")
                      []
                      (AST.GQLNamedType "String")
                      []
                  ]
              )
        it "parses with implements clause" $ do
          parse
            typeDefinition
            ""
            [r|"describe"
              interface NamedEntity implements ValueEntity {
                   name: String
              }
          |]
            `shouldParse` AST.TypeDefinitionInterface
              ( AST.InterfaceTypeDefinition
                  (Just $ AST.Description "describe")
                  (AST.Name "NamedEntity")
                  [AST.NamedType "ValueEntity"]
                  []
                  [ AST.FieldDefinition
                      Nothing
                      (AST.Name "name")
                      []
                      (AST.GQLNamedType "String")
                      []
                  ]
              )
        it "parses with multiple implements clauses" $ do
          parse
            typeDefinition
            ""
            [r|"describe"
              interface NamedEntity implements & ValueEntity & Node {
                   name: String
              }
          |]
            `shouldParse` AST.TypeDefinitionInterface
              ( AST.InterfaceTypeDefinition
                  (Just $ AST.Description "describe")
                  (AST.Name "NamedEntity")
                  [AST.NamedType "ValueEntity", AST.NamedType "Node"]
                  []
                  [ AST.FieldDefinition
                      Nothing
                      (AST.Name "name")
                      []
                      (AST.GQLNamedType "String")
                      []
                  ]
              )
      context "object type definition" $ do
        it "parses without implement clause" $
          parse
            typeDefinition
            ""
            [r|type Person {
            name: String
            age: Int
            picture: Url
          }|]
            `shouldParse` AST.TypeDefinitionObject
              ( AST.ObjectTypeDefinition
                  Nothing
                  (AST.Name "Person")
                  []
                  []
                  [ AST.FieldDefinition
                      Nothing
                      (AST.Name "name")
                      []
                      (AST.GQLNamedType "String")
                      [],
                    AST.FieldDefinition
                      Nothing
                      (AST.Name "age")
                      []
                      (AST.GQLNamedType "Int")
                      [],
                    AST.FieldDefinition
                      Nothing
                      (AST.Name "picture")
                      []
                      (AST.GQLNamedType "Url")
                      []
                  ]
              )
        it "parses with implement clause" $
          parse
            typeDefinition
            ""
            [r|type Person implements NameEntity {
            name: String
          }|]
            `shouldParse` AST.TypeDefinitionObject
              ( AST.ObjectTypeDefinition
                  Nothing
                  (AST.Name "Person")
                  [AST.NamedType "NameEntity"]
                  []
                  [ AST.FieldDefinition
                      Nothing
                      (AST.Name "name")
                      []
                      (AST.GQLNamedType "String")
                      []
                  ]
              )
        it "parses with implement clause and directives" $
          parse
            typeDefinition
            ""
            [r|type Person implements NameEntity @includes {
            name: String
          }|]
            `shouldParse` AST.TypeDefinitionObject
              ( AST.ObjectTypeDefinition
                  Nothing
                  (AST.Name "Person")
                  [AST.NamedType "NameEntity"]
                  [AST.Directive (AST.Name "includes") []]
                  [ AST.FieldDefinition
                      Nothing
                      (AST.Name "name")
                      []
                      (AST.GQLNamedType "String")
                      []
                  ]
              )
      context "union type definition" $ do
        it "parses union of one type" $ do
          parse
            typeDefinition
            ""
            [r|union SearchResult = | Photo|]
            `shouldParse` AST.TypeDefinitionUnion
              ( AST.UnionTypeDefinition
                  Nothing
                  (AST.Name "SearchResult")
                  []
                  [AST.NamedType "Photo"]
              )
        it "parses union of two types" $ do
          parse
            typeDefinition
            ""
            [r|union SearchResult = Photo | Person|]
            `shouldParse` AST.TypeDefinitionUnion
              ( AST.UnionTypeDefinition
                  Nothing
                  (AST.Name "SearchResult")
                  []
                  [AST.NamedType "Photo", AST.NamedType "Person"]
              )
        it "supports directives" $ do
          parse
            typeDefinition
            ""
            [r|union SearchResult @directive = Photo | Person|]
            `shouldParse` AST.TypeDefinitionUnion
              ( AST.UnionTypeDefinition
                  Nothing
                  (AST.Name "SearchResult")
                  [AST.Directive (AST.Name "directive") []]
                  [AST.NamedType "Photo", AST.NamedType "Person"]
              )
      context "enum type definition" $ do
        it "parses enum of one" $ do
          parse
            typeDefinition
            ""
            [r|enum Direction {
              NORTH
            }|]
            `shouldParse` AST.TypeDefinitionEnum
              ( AST.EnumTypeDefinition
                  Nothing
                  (AST.Name "Direction")
                  []
                  [AST.EnumValueDefinition Nothing (AST.EnumValue "NORTH") []]
              )
        it "parses union of two" $ do
          parse
            typeDefinition
            ""
            [r|enum Direction {
              NORTH
              EAST
            }|]
            `shouldParse` AST.TypeDefinitionEnum
              ( AST.EnumTypeDefinition
                  Nothing
                  (AST.Name "Direction")
                  []
                  [ AST.EnumValueDefinition Nothing (AST.EnumValue "NORTH") [],
                    AST.EnumValueDefinition Nothing (AST.EnumValue "EAST") []
                  ]
              )
        it "supports directives" $ do
          parse
            typeDefinition
            ""
            [r|enum Direction @directive {
              NORTH
              EAST @innerdirective
            }|]
            `shouldParse` AST.TypeDefinitionEnum
              ( AST.EnumTypeDefinition
                  Nothing
                  (AST.Name "Direction")
                  [AST.Directive (AST.Name "directive") []]
                  [ AST.EnumValueDefinition
                      Nothing
                      (AST.EnumValue "NORTH")
                      [],
                    AST.EnumValueDefinition
                      Nothing
                      (AST.EnumValue "EAST")
                      [AST.Directive (AST.Name "innerdirective") []]
                  ]
              )
      context "input object type definition" $ do
        it "parses without directives and arguments" $
          parse
            typeDefinition
            ""
            [r|input Point2D {
              x: Float
              y: Float
            }
            |]
            `shouldParse` AST.TypeDefinitionInputObject
              ( AST.InputObjectTypeDefinition
                  Nothing
                  (AST.Name "Point2D")
                  []
                  [ AST.InputValueDefinition
                      Nothing
                      (AST.Name "x")
                      (AST.GQLNamedType "Float")
                      Nothing
                      [],
                    AST.InputValueDefinition
                      Nothing
                      (AST.Name "y")
                      (AST.GQLNamedType "Float")
                      Nothing
                      []
                  ]
              )
        it "parses with directives and default value" $
          parse
            typeDefinition
            ""
            [r|input Point2D @directive {
              x: Float = 3
              y: Float
            }
            |]
            `shouldParse` AST.TypeDefinitionInputObject
              ( AST.InputObjectTypeDefinition
                  Nothing
                  (AST.Name "Point2D")
                  [AST.Directive (AST.Name "directive") []]
                  [ AST.InputValueDefinition
                      Nothing
                      (AST.Name "x")
                      (AST.GQLNamedType "Float")
                      (Just $ AST.VInt 3)
                      [],
                    AST.InputValueDefinition
                      Nothing
                      (AST.Name "y")
                      (AST.GQLNamedType "Float")
                      Nothing
                      []
                  ]
              )
