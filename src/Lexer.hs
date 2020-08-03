module Lexer where

import Control.Applicative
  ( Alternative (..),
    (*>),
    (<*),
    (<*>),
    pure,
    (<$>)
  )
import Control.Monad ((>>))
import Data.Char (Char, isAsciiLower, isAsciiUpper)
import Data.Bool (Bool, (||))
import Data.Eq ((/=))
import Data.Maybe (Maybe(..))
import Data.Function (($))
import GHC.Num (Num, (+), Integer)
import GHC.Float (Double)
import GHC.Real (fromIntegral)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec,
    between,
    optional,
    skipSome,
    parseTest,
    eof,
    notFollowedBy,
    satisfy,
    choice,
    takeWhileP,
    manyTill,
    oneOf
  )
import Text.Megaparsec.Char
  ( char,
    digitChar,
    space1,
    numberChar,
    string,
    
  )
import qualified Text.Megaparsec.Char.Lexer as L

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

spaceConsumer :: Parser ()
spaceConsumer = L.space ignoredChars comment empty


-- Lexing Primitives
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: T.Text -> Parser T.Text
symbol = L.symbol spaceConsumer

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


-- Name 
name :: Parser T.Text
name = T.cons <$> nameStart <*> (T.pack <$> many (letter <|> digitChar <|> char '_'))

letter :: Parser Char
letter = satisfy w
  where
    w = \c -> isAsciiLower c || isAsciiUpper c

nameStart :: Parser Char
nameStart = letter <|> char '_'


-- Numbers
intVal :: Parser Integer
intVal = L.signed (pure ()) $ lexeme integerPart 

integerPart :: Parser Integer
integerPart = (zero <|> L.decimal) <* notFollowedBy (digitChar <|> nameStart <|> char '.')

zero :: (Num n) => Parser n
zero = char '0' *> pure 0 

-- This is not really according to spec
floatVal :: Parser Double
floatVal = L.signed (pure ()) L.float


stringValue :: Parser T.Text
stringValue = char '"' *> (T.pack <$> manyTill stringChar (char '"'))
  where
    stringChar = choice [satisfy (\c -> c /= '\\'), escapedChar]
    escapedChar = char '\\' *> oneOf ['"', '\\', '/', 'b', 'f', 'n', 'r', 't']