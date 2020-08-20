{-# LANGUAGE QuasiQuotes #-}

module LexerSpec
  ( spec,
  )
where

import Control.Applicative (pure)
import Data.Either (Either)
import Data.Function (($))
import Data.Int (Int)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import GHC.Num (Integer)
import Lexer
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
spec = describe "Lexer" $ do
  context "Lexers" $ do
    it "accepts BOM" $ parse unicodeBOM "" `shouldSucceedOn` "\xfeff"

    it "lexes punctuation" $ do
      parse bang "" "!" `shouldParse` "!"
      parse dollarSign "" "$    " `shouldParse` "$"
      runParser' dollarSign (initialState "$   2") `succeedsLeaving` "2"
      parse threedots "" "..." `shouldParse` "..."
      parse equal "" "= " `shouldParse` "="
      parse atSymbol "" "@" `shouldParse` "@"
      parse pipe "" "|" `shouldParse` "|"
      runBetween parens `shouldSucceedOn` "(    )"
      runBetween squareBrackets `shouldSucceedOn` "[    ]"
      runBetween brackets `shouldSucceedOn` "{    }"
      parse (brackets bang) "" `shouldSucceedOn` "{ ! }"

    it "lexes strings" $ do
      parse stringVal "" [r|" white space "|] `shouldParse` " white space "
      parse stringVal "" [r|"\\"|] `shouldParse` "\\"
      parse stringVal "" [r|"escaped \n"|] `shouldParse` "escaped \n"
      parse stringVal "" `shouldFailOn` [r|"\g"|]
      parse stringVal "" `shouldFailOn` [r|"\  "|]
      -- Fix unicode escaping case
      parse stringVal "" `shouldFailOn` [r|"\u0020"|]

    it "lexes block strings" $ do
      parse stringVal "" [r|"""Block string"""|] `shouldParse` "Block string"
      parse
        stringVal
        ""
        [r|"""
        Hello,
          World!
    
        Yours,
          GraphQL.
      """|]
        `shouldParse` "Hello,\n  World!\n\nYours,\n  GraphQL."

    it "lexes integer" $ do
      parse intVal "" "4" `shouldParse` (4 :: Integer)
      parse intVal "" "-4" `shouldParse` (-4 :: Integer)
      parse intVal "" "92" `shouldParse` (92 :: Integer)
      parse intVal "" "0" `shouldParse` (0 :: Integer)
      parse intVal "" `shouldFailOn` "04"
      parse intVal "" "-0" `shouldParse` (0 :: Integer)
      parse intVal "" `shouldFailOn` "-  4"
      runParser' intVal (initialState "4a") `failsLeaving` "a"
      runParser' intVal (initialState "4.22") `failsLeaving` ".22"
      runParser' intVal (initialState "4  .22") `succeedsLeaving` ".22"

    it "lexes floats" $ do
      parse floatVal "" "-4.123" `shouldParse` (-4.123)
      parse floatVal "" "0.123" `shouldParse` 0.123
      -- Have to fix this case
      parse floatVal "" "00.123" `shouldParse` 00.123
      parse floatVal "" "123e4" `shouldParse` 123e4
      parse floatVal "" "123E4" `shouldParse` 123E4
      parse floatVal "" "123e-4" `shouldParse` 123e-4
      parse floatVal "" "123e+4" `shouldParse` 123e+4
      parse floatVal "" "-1.123e4" `shouldParse` (-1.123e4)
      parse floatVal "" "-1.123E4" `shouldParse` (-1.123E4)
      parse floatVal "" "-1.123e-4" `shouldParse` (-1.123e-4)
      parse floatVal "" "-1.123e+4" `shouldParse` (-1.123e+4)
      parse floatVal "" "-1.123e4567" `shouldParse` (-1.123e4567)

runBetween ::
  (Parser () -> Parser ()) -> Text -> Either (ParseErrorBundle Text Void) ()
runBetween parser = parse (parser $ pure ()) ""
