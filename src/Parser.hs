{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Parser where

import qualified AST
import Control.Applicative (pure, (*>), (<$), (<$>), (<*), (<*>))
import Data.Function (($), (.))
import Lexer (Parser)
import qualified Lexer as Lexer
import Text.Megaparsec hiding (State)

document :: Parser AST.Document
document = AST.DocumentOperation <$> operation

operationType :: Parser AST.OperationType
operationType =
  AST.Query <$ Lexer.symbol "query"
    <|> AST.Mutation <$ Lexer.symbol "mutation"
    <|> AST.Subscription <$ Lexer.symbol "subscription"

operation :: Parser AST.Operation
operation = AST.Operation <$> operationType <*> optional name <*> variableDefinitions <*> selectionSet

variableDefinitions :: Parser [AST.VariableDefinition]
variableDefinitions = (Lexer.parens $ some variableDefinition) <|> pure []

variableDefinition :: Parser AST.VariableDefinition
variableDefinition = AST.VariableDefinition <$> variable <* Lexer.colon <*> gQLType

variable :: Parser AST.Variable
variable = AST.Variable <$> (Lexer.dollarSign *> name)

field :: Parser AST.Field
field = AST.Field <$> name <*> arguments <*> (selectionSet <|> pure [])

name :: Parser AST.Name
name = AST.Name <$> Lexer.name

selectionSet :: Parser AST.SelectionSet
selectionSet = Lexer.brackets $ some selection

selection :: Parser AST.Selection
selection = AST.SelectionField <$> field

arguments :: Parser [AST.Argument]
arguments = (Lexer.parens $ some argument) <|> pure []

argument :: Parser AST.Argument
argument = AST.Argument <$> name <* Lexer.colon <*> value

value :: Parser AST.Value
value = AST.VVariable <$> variable

-- Types
gQLType :: Parser AST.GQLType
gQLType = namedType

namedType :: Parser AST.GQLType
namedType = AST.NamedType <$> name