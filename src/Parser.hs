{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Parser where

-- import           Control.Applicative
import           Control.Monad
import           Data.Text                      ( Text )
import           Data.Void
import           Text.Megaparsec         hiding ( State )
import           Text.Megaparsec.Char
import           Text.Megaparsec.Debug
import qualified Data.Text                     as T
import qualified Text.Megaparsec.Char.Lexer    as L

type Parser = Parsec Void Text
