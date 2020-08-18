module Lexer where

import Control.Applicative
  ( Alternative (..),
    pure,
    (*>),
    (<$),
    (<$>),
    (<*),
    (<*>),
  )
import Control.Monad ((>>))
import Data.Bool ((||))
import Data.Char (Char, isAsciiLower, isAsciiUpper)
import Data.Eq ((==))
import Data.Foldable (foldr')
import Data.Function (($), (.))
import Data.Functor (($>))
import Data.Int (Int)
import Data.List (init, last, map)
import Data.Maybe (Maybe (..))
import Data.Ord ((<), (>))
import qualified Data.Text as T
import Data.Void (Void)
import GHC.Float (Double)
import GHC.Num (Integer, Num)
import Text.Megaparsec
  ( Parsec,
    anySingle,
    between,
    choice,
    manyTill,
    notFollowedBy,
    optional,
    satisfy,
    skipSome,
  )
import Text.Megaparsec.Char
  ( char,
    digitChar,
    newline,
    printChar,
    space1,
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
name = lexeme $ T.cons <$> nameStart <*> (T.pack <$> many (letter <|> digitChar <|> char '_'))

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
stringValue = blockString <|> singleLine
  where
    singleLine = char '"' *> (T.pack <$> manyTill stringChar (char '"'))
    stringChar = do
      x <- printChar
      case x of
        '\\' -> choice escapeChars
        x -> pure x

escapeChars :: [Parser Char]
escapeChars = map codeToReplacement escapeCharAndReplacements
  where
    -- TODO: support unicode code escapes
    codeToReplacement :: (Char, Char) -> Parser Char
    codeToReplacement (code, replacement) = (char code $> replacement)
    escapeCharAndReplacements =
      [ ('b', '\b'),
        ('n', '\n'),
        ('f', '\f'),
        ('r', '\r'),
        ('t', '\t'),
        ('\\', '\\'),
        ('\"', '\"'),
        ('/', '/')
      ]

blockString :: Parser T.Text
blockString = string "\"\"\"" *> (formatBlockString . T.pack <$> manyTill anySingle (string "\"\"\""))

formatBlockString :: T.Text -> T.Text
formatBlockString x =
  case T.lines x of
    [] -> ""
    (z : []) -> z
    (firstLine : xs) ->
      -- We have to drop last extra \n from unlines
      (T.dropEnd 1)
        . T.unlines
        . removeLastLineIfItsOnlySpace
        . removeFirstLineIfItsOnlySpace firstLine
        $ formatCommonIndent xs
  where
    removeFirstLineIfItsOnlySpace z xs = if isOnlySpace z then xs else z : xs
    removeLastLineIfItsOnlySpace xs = if isOnlySpace $ last xs then init xs else xs
    -- Can I make this more efficient?
    isOnlySpace = T.all (== ' ')
    formatCommonIndent xs = case calculateCommonIndent xs of
      Nothing -> xs
      Just commonIndent -> map (T.drop commonIndent) xs

calculateCommonIndent :: [T.Text] -> Maybe Int
calculateCommonIndent xs = foldr' accCommonIndet Nothing xs
  where
    getIndent = T.length . (T.takeWhile (== ' '))
    accCommonIndet line commonIndent =
      let indent = getIndent line
       in if T.length line > indent
            then Just $ case commonIndent of
              Nothing -> indent
              Just n -> if indent < n then indent else n
            else commonIndent
