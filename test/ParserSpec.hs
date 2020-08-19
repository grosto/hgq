{-# LANGUAGE QuasiQuotes #-}

module ParserSpec
  ( spec,
  )
where

import AST
import Control.Applicative (pure)
import Data.Either (Either)
import Data.Function (($))
import Data.Int (Int)
import Data.Maybe (Maybe (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import GHC.Num (Integer)
import Parser
import Test.Hspec
  ( Spec,
    context,
    describe,
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
  context "operations" $ do
    fit "should parse operation with name" $ do
      parse
        document
        ""
        [r|mutation like { 
          like
         } |]
        `shouldParse` ( DocumentOperation $
                          Operation Mutation (Just "like") [] $
                            [SelectionField $ Field "like" [] []]
                      )
    fit "should parse operation with arguments" $ do
      parse
        document
        ""
        [r|query like($id: Int) { 
          likeStory (id: $id)
         } |]
        `shouldParse` ( DocumentOperation $
                          Operation
                            Query
                            (Just "like")
                            [VariableDefinition (Variable "id") (NamedType "Int")]
                            $ [SelectionField $ Field "likeStory" [Argument (Name "id") (VVariable (Variable "id"))] []]
                      )