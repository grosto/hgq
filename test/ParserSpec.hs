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
  context "Values Parser" $ do
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
  context "Const Values Parser" $ do
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
    it "should parse field with alias" $ do
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
    it "should parse field without alias" $ do
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
    it "should parse directives without arguments" $ do
      parse
        directives
        ""
        [r|@super @power|]
        `shouldParse` [ AST.Directive (AST.Name "super") [],
                        AST.Directive (AST.Name "power") []
                      ]
    it "should parse directives with arguments" $ do
      parse
        directives
        ""
        [r|@addExternalFields(source: "profiles") @excludeField(name: "photo")|]
        `shouldParse` [ AST.Directive (AST.Name "addExternalFields") [AST.Argument "source" (AST.VString "profiles")],
                        AST.Directive (AST.Name "excludeField") [AST.Argument "name" (AST.VString "photo")]
                      ]

  context "fragments" $ do
    it "parses fragment definition" $ do
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
    it "fragment name cannot be on" $ do
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
    it "should parse fragment spreads" $
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
    it "should parse inline fragments without type condition" $
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
    it "should parse inline fragments with type condition" $
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
  context "documents" $ do
    it "should parse operation with name" $ do
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
    it "should parse operation with arguments" $ do
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
    it "should parse operation with directives" $ do
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
    it "should parse top level fragment definition" $ do
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