module Lexer where


import           Control.Monad                  ( (>>) )
import           Control.Applicative            ( pure
                                                , Alternative(..)
                                                )
import           Text.Megaparsec                ( Parsec
                                                , optional
                                                , skipSome
                                                , between
                                                )
import qualified Text.Megaparsec.Char.Lexer    as L
import           Data.Void                      ( Void )
import qualified Data.Text                     as T
import           Data.Char                      ( Char )

import           Text.Megaparsec.Char           ( char
                                                , digitChar
                                                , space1
                                                )

type Parser = Parsec Void T.Text

-- Ignored

unicodeBOM :: Parser ()
unicodeBOM = optional (char '\xfeff') >> pure ()

ignoredChars :: Parser ()
ignoredChars = space1 <|> skipSome comma

comma :: Parser Char
comma = char ','

comment :: Parser ()
comment = L.skipLineComment "#"

space :: Parser ()
space = L.space ignoredChars comment empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: T.Text -> Parser T.Text
symbol = L.symbol space

-- Punctuators
dollarSign :: Parser T.Text
dollarSign = symbol "$"

bang :: Parser T.Text
bang = symbol "!"

threedots :: Parser T.Text
threedots = symbol "..."

colon :: Parser T.Text
colon = symbol ":"

equal :: Parser T.Text
equal = symbol "="

atSymbol :: Parser T.Text
atSymbol = symbol "@"

pipe :: Parser T.Text
pipe = symbol "|"

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "{") (symbol "}")

squareBrackets :: Parser a -> Parser a
squareBrackets = between (symbol "[") (symbol "]")
