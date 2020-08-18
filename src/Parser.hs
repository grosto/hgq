{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Parser where

import Control.Applicative (pure, (<$), (<$>), (<*>))
import Control.Monad
import Data.Eq (Eq)
import Data.Function (($))
import Data.Maybe (Maybe (..))
import Data.Ord (Ord)
import Data.String (IsString)
import qualified Data.Text as T
import GHC.Show (Show)
import Lexer (Parser)
import qualified Lexer as Lexer
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Debug

newtype Name = Name {unName :: T.Text}
  deriving (Eq, Ord, Show, IsString)

type Description = T.Text

data ScalarTypeDefinition = ScalarTypeDefinition
  { description :: Description,
    sTDName :: Name
  }

data Document = DocumentOperation Operation
  deriving (Show, Eq)

data OperationType = Query | Mutation | Subscription
  deriving (Show, Eq)

data Operation = Operation
  { oOperationType :: OperationType,
    oName :: Name,
    oSelectionSet :: SelectionSet
  }
  deriving (Show, Eq)

data SelectionSet = SelectionSet [Selection]
  deriving (Show, Eq)

data Selection = SelectionField Field
  deriving (Show, Eq)

data Field = Field
  { fname :: Name,
    fselectionSet :: Maybe SelectionSet
  }
  deriving (Show, Eq)

document :: Parser Document
document = DocumentOperation <$> operation

operationType :: Parser OperationType
operationType =
  Query <$ Lexer.symbol "query"
    <|> Mutation <$ Lexer.symbol "mutation"
    <|> Subscription <$ Lexer.symbol "subscription"

operation :: Parser Operation
operation = Operation <$> operationType <*> name <*> selectionSet

field :: Parser Field
field = Field <$> name <*> ((Just <$> selectionSet) <|> pure Nothing)

name :: Parser Name
name = Name <$> Lexer.name

selectionSet :: Parser SelectionSet
selectionSet = SelectionSet <$> (Lexer.brackets $ ((: []) <$> selection))

selection :: Parser Selection
selection = SelectionField <$> field