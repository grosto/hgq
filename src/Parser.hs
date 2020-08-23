{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Parser where

import qualified AST
import Control.Applicative (pure, (*>), (<$), (<$>), (<*), (<*>))
import Control.Monad.Fail (fail)
import Data.Bool (Bool (..))
import Data.Function (($))
import qualified Data.Map.Strict as Map
import Lexer (Parser)
import qualified Lexer as Lexer
import Text.Megaparsec hiding (State)

document :: Parser AST.Document
document = Lexer.spaceConsumer *> (AST.DocumentOperation <$> operation)

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
name = AST.Name <$> Lexer.name <?> "Name"

selectionSet :: Parser AST.SelectionSet
selectionSet = Lexer.brackets $ some selection

selection :: Parser AST.Selection
selection = AST.SelectionField <$> field

arguments :: Parser [AST.Argument]
arguments = (Lexer.parens $ some argument) <|> pure []

argument :: Parser AST.Argument
argument = AST.Argument <$> name <* Lexer.colon <*> value

value :: Parser AST.Value
value =
  choice
    [ AST.VBool <$> boolVal,
      AST.VNull <$ Lexer.symbol "null",
      AST.VVariable <$> variable,
      try $ AST.VFloat <$> Lexer.floatVal,
      AST.VInt <$> Lexer.intVal,
      AST.VString <$> Lexer.stringVal,
      AST.VList <$> listG value,
      AST.VObject <$> objectG value
    ]

valueConst :: Parser AST.ValueConst
valueConst =
  choice
    [ AST.VCBool <$> boolVal,
      AST.VCNull <$ Lexer.symbol "null",
      try $ AST.VCFloat <$> Lexer.floatVal,
      AST.VCInt <$> Lexer.intVal,
      AST.VCString <$> Lexer.stringVal,
      AST.VCList <$> listG valueConst,
      AST.VCObject <$> objectG valueConst
    ]

listG :: Parser a -> Parser [a]
listG val = Lexer.squareBrackets $ many val

objectG :: Parser a -> Parser (Map.Map AST.Name a)
-- I can write this more efficiently for sure
objectG val = Lexer.brackets $ Map.fromList <$> (many $ objectFieldG val)

objectFieldG :: Parser a -> Parser (AST.Name, a)
objectFieldG val = (,) <$> name <* Lexer.colon <*> val

boolVal :: Parser Bool
boolVal = True <$ Lexer.symbol "true" <|> False <$ Lexer.symbol "false"

-- Types
gQLType :: Parser AST.GQLType
gQLType =
  try nonNullableTypes
    <|> (AST.GQLNamedType <$> namedType)
    <|> listType
  where
    nonNullableTypes = AST.NonNullType <$> nonNullType

namedType :: Parser AST.NamedType
namedType = AST.NamedType <$> name

listType :: Parser AST.GQLType
listType = AST.ListType <$> Lexer.squareBrackets gQLType

nonNullType :: Parser AST.NonNullGQLType
nonNullType = (nonNullNamedType <|> nonNullListType)

nonNullNamedType :: Parser AST.NonNullGQLType
nonNullNamedType = AST.NonNullNamedType <$> (name <* Lexer.bang)

nonNullListType :: Parser AST.NonNullGQLType
nonNullListType = AST.NonNullListType <$> Lexer.squareBrackets gQLType <* Lexer.bang

-- Fragments
fragment :: Parser AST.FragmentDefinition
fragment =
  AST.FragmentDefinition
    <$> (Lexer.symbol "fragment" *> fragmentName)
      <*> typeCondition
      <*> (pure [])
      <*> selectionSet

fragmentName :: Parser AST.FragmentName
fragmentName =
  lookAhead (Lexer.symbol "on" *> Lexer.allowedNameChars)
    *> (AST.FragmentName <$> Lexer.name)

typeCondition :: Parser AST.TypeCondition
typeCondition = AST.TypeCondition <$> (Lexer.symbol "on" *> namedType)