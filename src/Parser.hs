module Parser where


import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Data.Text                     as T
import           Data.Void                      ( Void )

type Parser = Parsec Void T.Text

newline :: Parser Char
newline = single '\n'
