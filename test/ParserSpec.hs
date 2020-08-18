{-# LANGUAGE QuasiQuotes #-}

module ParserSpec
  ( spec,
  )
where

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
    fit "should parse mutation without arguments" $ do
      parse
        document
        ""
        [r|mutation like { 
          likeStory
         } |]
        `shouldParse` ( DocumentOperation $
                          Operation Mutation "like" $
                            SelectionSet $
                              [SelectionField $ Field "likeStory" Nothing]
                      )