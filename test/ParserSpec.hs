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
  fcontext "types" $ do
    it "parses named type" $ do
      parse gQLType "" "Int" `shouldParse` (AST.NamedType "Int")
    it "parses list type" $ do
      parse gQLType "" "[Int]" `shouldParse` (AST.ListType $ AST.NamedType "Int")
    it "parses no-null type" $ do
      parse gQLType "" "Int!" `shouldParse` (AST.NonNullType $ AST.NonNullNamedType "Int")
    it "parses no-null list type" $ do
      parse gQLType "" "[Int]!"
        `shouldParse` (AST.NonNullType $ AST.NonNullListType $ AST.NamedType "Int")
    it "parses no-null list type with non-null named type inside" $ do
      parse gQLType "" "[Int!]!"
        `shouldParse` (AST.NonNullType $ AST.NonNullListType $ AST.NonNullType $ AST.NonNullNamedType "Int")
  context "operations" $ do
    it "should parse operation with name" $ do
      parse
        document
        ""
        [r|mutation like { 
          like
         } |]
        `shouldParse` ( AST.DocumentOperation $
                          AST.Operation AST.Mutation (Just "like") [] $
                            [AST.SelectionField $ AST.Field "like" [] []]
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
                            [AST.VariableDefinition (AST.Variable "id") (AST.NamedType "Int")]
                            $ [AST.SelectionField $ AST.Field "likeStory" [AST.Argument (AST.Name "id") (AST.VVariable (AST.Variable "id"))] []]
                      )
