module Parser where

import qualified AST
import Control.Applicative (pure, (*>), (<$), (<$>), (<*), (<*>))
import Data.Bool (Bool (..))
import Data.Function (($))
import qualified Data.Map.Strict as Map
import Data.Maybe (Maybe (..))
import qualified Data.Text as T
import Lexer (Parser)
import qualified Lexer as Lexer
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char (string)

name :: Parser AST.Name
name = AST.Name <$> Lexer.name <?> "Name"

description :: Parser AST.Description
description = AST.Description <$> Lexer.stringVal <?> "Description"

keywordGuard :: [T.Text] -> Parser ()
keywordGuard keywords = notFollowedBy ((choice $ string <$> keywords) *> notFollowedBy Lexer.allowedNameChars)

document :: Parser AST.Document
document =
  Lexer.spaceConsumer
    *> (AST.DocumentOperation <$> operation <|> AST.DocumentFragment <$> fragment)

operationType :: Parser AST.OperationType
operationType =
  AST.Query <$ Lexer.symbol "query"
    <|> AST.Mutation <$ Lexer.symbol "mutation"
    <|> AST.Subscription <$ Lexer.symbol "subscription"

operation :: Parser AST.Operation
operation = AST.Operation <$> operationType <*> optional name <*> variableDefinitions <*> directives <*> selectionSet

variableDefinitions :: Parser [AST.VariableDefinition]
variableDefinitions = (Lexer.parens $ some variableDefinition) <|> pure []

variableDefinition :: Parser AST.VariableDefinition
variableDefinition = AST.VariableDefinition <$> variable <* Lexer.colon <*> gQLType

variable :: Parser AST.Variable
variable = AST.Variable <$> (Lexer.dollarSign *> name)

field :: Parser AST.Field
field = createField <$> nameWithPossibleAlias <*> arguments <*> (selectionSet <|> pure [])
  where
    createField =
      ( \(parsedName, parsedAlias) parsedArguments parsedSelectionSet ->
          AST.Field
            parsedName
            parsedArguments
            parsedSelectionSet
            parsedAlias
      )
    nameWithPossibleAlias :: Parser (AST.Name, Maybe AST.Name)
    nameWithPossibleAlias = mapNameAndAlias <$> name <*> (optional $ Lexer.colon *> name)
    mapNameAndAlias a b = case b of
      Just parsedName -> (parsedName, Just a)
      Nothing -> (a, b)

selectionSet :: Parser AST.SelectionSet
selectionSet = Lexer.brackets $ some selection

selection :: Parser AST.Selection
selection =
  choice
    [ AST.SelectionField <$> field,
      AST.SelectionFragmentSpread <$> try fragmentSpread,
      AST.SelectionInlineFragment <$> inlineFragment
    ]
  where
    inlineFragment :: Parser AST.InlineFragment
    inlineFragment = AST.InlineFragment <$> (Lexer.threedots *> (optional $ typeCondition)) <*> directives <*> selectionSet

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
-- I can for sure write this more efficiently
objectG val = Lexer.brackets $ Map.fromList <$> (many $ objectFieldG val)

objectFieldG :: Parser a -> Parser (AST.Name, a)
objectFieldG val = (,) <$> name <* Lexer.colon <*> val

boolVal :: Parser Bool
boolVal = True <$ Lexer.symbol "true" <|> False <$ Lexer.symbol "false"

enumValue :: Parser AST.EnumValue
enumValue = AST.EnumValue <$> (guards *> Lexer.name)
  where
    guards =
      keywordGuard $ ["true", "false", "null"]

-- Fragments
fragment :: Parser AST.FragmentDefinition
fragment =
  AST.FragmentDefinition
    <$> (Lexer.symbol "fragment" *> fragmentName)
      <*> typeCondition
      <*> (pure [])
      <*> selectionSet

fragmentName :: Parser AST.FragmentName
fragmentName = AST.FragmentName <$> (keywordGuard (["on"]) *> Lexer.name)

typeCondition :: Parser AST.TypeCondition
typeCondition = AST.TypeCondition <$> (Lexer.symbol "on" *> namedType)

fragmentSpread :: Parser AST.FragmentSpread
fragmentSpread = AST.FragmentSpread <$> (Lexer.threedots *> fragmentName) <*> directives

-- Directives
directives :: Parser AST.Directives
directives = many directive

directive :: Parser AST.Directive
directive = AST.Directive <$> (Lexer.atSymbol *> name) <*> arguments

-- Type references
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

-- Type system

schemaDefinition :: Parser AST.SchemaDefinition
schemaDefinition =
  AST.SchemaDefinition
    <$> optional description
      <*> (Lexer.symbol "schema" *> directives)
      <*> (Lexer.brackets (some rootOperationTypeDefinition))

rootOperationTypeDefinition :: Parser AST.RootOperationTypeDefinition
rootOperationTypeDefinition =
  AST.RootOperationTypeDefinition
    <$> operationType
    <*> (Lexer.colon *> namedType)

typeDefinition :: Parser AST.TypeDefinition
typeDefinition =
  choice $
    try
      <$> [ AST.TypeDefinitionScalar <$> scalarTypeDefinition,
            AST.TypeDefinitionInterface <$> interfaceTypeDefinition,
            AST.TypeDefinitionObject <$> objectTypeDefinition,
            AST.TypeDefinitionUnion <$> unionTypeDefinition,
            AST.TypeDefinitionEnum <$> enumTypeDefinition
          ]

scalarTypeDefinition :: Parser AST.ScalarTypeDefinition
scalarTypeDefinition =
  AST.ScalarTypeDefinition
    <$> optional description
      <*> (Lexer.symbol "scalar" *> name)
      <*> directives

interfaceTypeDefinition :: Parser AST.InterfaceTypeDefinition
interfaceTypeDefinition =
  AST.InterfaceTypeDefinition
    <$> optional description
    <*> (Lexer.symbol "interface" *> name)
    <*> optionalImplementsInterfaces
    <*> directives
    <*> optionalFieldsDefinition

objectTypeDefinition :: Parser AST.ObjectTypeDefinition
objectTypeDefinition =
  AST.ObjectTypeDefinition
    <$> optional description
    <*> (Lexer.symbol "type" *> name)
    <*> optionalImplementsInterfaces
    <*> directives
    <*> fieldsDefintion

unionTypeDefinition :: Parser AST.UnionTypeDefinition
unionTypeDefinition =
  AST.UnionTypeDefinition
    <$> optional description
    <*> (Lexer.symbol "union" *> name)
    <*> directives
    <*> (unionMemberTypes <|> pure [])

enumTypeDefinition :: Parser AST.EnumTypeDefinition
enumTypeDefinition =
  AST.EnumTypeDefinition
    <$> optional description
    <*> (Lexer.symbol "enum" *> name)
    <*> directives
    <*> enumValuesDefinition

optionalImplementsInterfaces :: Parser AST.ImplementsInterfaces
optionalImplementsInterfaces = (implementsInterfaces <|> pure [])

implementsInterfaces :: Parser AST.ImplementsInterfaces
implementsInterfaces =
  ((:) <$> ((Lexer.symbol "implements" *> optional Lexer.and) *> namedType))
    <*> many (Lexer.and *> namedType)

unionMemberTypes :: Parser AST.UnionMemberTypes
unionMemberTypes =
  ((:) <$> ((Lexer.symbol "=" *> optional Lexer.pipe) *> namedType))
    <*> many (Lexer.pipe *> namedType)

enumValuesDefinition :: Parser [AST.EnumValueDefinition]
enumValuesDefinition = Lexer.brackets (some enumValueDefinition)
  where
    enumValueDefinition =
      AST.EnumValueDefinition <$> optional description <*> enumValue <*> directives

fieldsDefintion :: Parser AST.FieldsDefinition
fieldsDefintion = Lexer.brackets (some fieldDefinition)

optionalFieldsDefinition :: Parser AST.FieldsDefinition
optionalFieldsDefinition = fieldsDefintion <|> pure []

fieldDefinition :: Parser AST.FieldDefinition
fieldDefinition =
  AST.FieldDefinition
    <$> optional description
    <*> name
    <*> (argumentsDefinition <|> pure [])
    <*> (Lexer.colon *> gQLType)
    <*> directives

argumentsDefinition :: Parser AST.ArgumentsDefinition
argumentsDefinition = Lexer.parens (some inputValueDefinition)

inputValueDefinition :: Parser AST.InputValueDefinition
inputValueDefinition =
  AST.InputValueDefinition
    <$> optional description
    <*> name
    <*> (Lexer.colon *> gQLType)
    <*> optionalDefaultValue
    <*> directives

optionalDefaultValue :: Parser AST.DefaultValue
optionalDefaultValue = optional $ Lexer.equal *> value
