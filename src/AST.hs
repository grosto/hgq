{-# LANGUAGE DerivingVia #-}

module AST where

import Data.Bool (Bool)
import Data.Eq (Eq)
import qualified Data.Map.Strict as Map
import Data.Maybe (Maybe (..))
import Data.Ord (Ord)
import Data.String (IsString)
import qualified Data.Text as T
import GHC.Float (Double)
import GHC.Num (Integer)
import GHC.Show (Show)

newtype Name = Name
  {unName :: T.Text}
  deriving
    (Eq, Ord, Show, IsString)
    via T.Text

newtype Description = Description
  {unDescription :: T.Text}
  deriving
    (Eq, Ord, Show, IsString)
    via T.Text

data Document = DocumentOperation Operation | DocumentFragment FragmentDefinition
  deriving (Show, Eq)

data OperationType = Query | Mutation | Subscription
  deriving (Show, Eq)

data Operation = Operation
  { oOperationType :: OperationType,
    oName :: Maybe Name,
    oVariableDefinitons :: [VariableDefinition],
    oDirectives :: Directives,
    oSelectionSet :: SelectionSet
  }
  deriving (Show, Eq)

data VariableDefinition = VariableDefinition
  { vVariableDefinition :: Variable,
    vType :: GQLType
  }
  deriving (Show, Eq)

newtype Variable = Variable
  { unVariable :: Name
  }
  deriving (Show, Eq)

type SelectionSet = [Selection]

data Selection = SelectionField Field | SelectionFragmentSpread FragmentSpread | SelectionInlineFragment InlineFragment
  deriving (Show, Eq)

data Field = Field
  { fname :: Name,
    fArguments :: [Argument],
    fselectionSet :: SelectionSet,
    alias :: Maybe Name
  }
  deriving (Show, Eq)

data Argument = Argument
  { aName :: Name,
    aValue :: Value
  }
  deriving (Show, Eq)

data Value
  = VVariable Variable
  | VInt Integer
  | VFloat Double
  | VString T.Text
  | VBool Bool
  | VNull
  | VList [Value]
  | VObject (Map.Map Name Value)
  deriving (Show, Eq)

data ValueConst
  = VCInt Integer
  | VCFloat Double
  | VCString T.Text
  | VCBool Bool
  | VCNull
  | VCList [ValueConst]
  | VCObject (Map.Map Name ValueConst)
  deriving (Show, Eq)

newtype EnumValue = EnumValue
  { unEnumValue :: T.Text
  }
  deriving
    (Eq, Ord, Show, IsString)
    via T.Text

-- Fragments
data FragmentDefinition = FragmentDefinition
  { fName :: FragmentName,
    fTypeCondition :: TypeCondition,
    fDirectives :: Directives,
    fSelectionSet :: SelectionSet
  }
  deriving
    (Eq, Show)

data InlineFragment = InlineFragment
  { iTypeCondition :: Maybe TypeCondition,
    iDirectives :: Directives,
    iSelectionSet :: SelectionSet
  }
  deriving
    (Eq, Show)

newtype FragmentName = FragmentName {unFragmentName :: T.Text}
  deriving
    (Eq, Ord, Show, IsString)
    via T.Text

newtype TypeCondition = TypeCondition NamedType
  deriving
    (Eq, Ord, Show, IsString)
    via NamedType

data FragmentSpread = FragmentSpread
  { fsName :: FragmentName,
    fsDirectives :: Directives
  }
  deriving
    (Eq, Show)

-- Directives
type Directives = [Directive]

data Directive = Directive
  { dName :: Name,
    dArguments :: [Argument]
  }
  deriving
    (Eq, Show)

-- type references
data GQLType = GQLNamedType NamedType | ListType GQLType | NonNullType NonNullGQLType
  deriving (Show, Eq)

newtype NamedType = NamedType Name
  deriving
    (Eq, Ord, Show, IsString)
    via Name

data NonNullGQLType = NonNullNamedType Name | NonNullListType GQLType
  deriving (Show, Eq)

-- type system
data SchemaDefinition = SchemaDefinition
  { sDescription :: Maybe Description,
    sDirectives :: Directives,
    sRootOperationTypeDefinitions :: [RootOperationTypeDefinition]
  }
  deriving (Show, Eq)

data RootOperationTypeDefinition = RootOperationTypeDefinition
  { rOperationType :: OperationType,
    rType :: NamedType
  }
  deriving (Show, Eq)

data TypeDefinition
  = TypeDefinitionScalar ScalarTypeDefinition
  | TypeDefinitionObject ObjectTypeDefinition
  | TypeDefinitionInterface InterfaceTypeDefinition
  | TypeDefinitionUnion UnionTypeDefinition
  | TypeDefinitionEnum EnumTypeDefinition
  deriving (Show, Eq)

data ScalarTypeDefinition = ScalarTypeDefinition
  { stdescription :: Maybe Description,
    stName :: Name,
    stDirectives :: Directives
  }
  deriving (Show, Eq)

data ObjectTypeDefinition = ObjectTypeDefinition
  { otDescription :: Maybe Description,
    otName :: Name,
    otImplementsInterfaces :: ImplementsInterfaces,
    otDirectives :: Directives,
    otFieldsDefinition :: FieldsDefinition
  }
  deriving (Show, Eq)

type ImplementsInterfaces = [NamedType]

data InterfaceTypeDefinition = InterfaceTypeDefinition
  { itDescription :: Maybe Description,
    itName :: Name,
    itImplementsInterfaces :: ImplementsInterfaces,
    itDirectives :: Directives,
    itFieldsDefinitions :: FieldsDefinition
  }
  deriving (Show, Eq)

type UnionMemberTypes = [NamedType]

data UnionTypeDefinition = UnionTypeDefinition
  { utDescription :: Maybe Description,
    utName :: Name,
    utDirectives :: Directives,
    utUnionMemberTypes :: UnionMemberTypes
  }
  deriving (Show, Eq)

data EnumTypeDefinition = EnumTypeDefinition
  { etDescription :: Maybe Description,
    etName :: Name,
    etDirectives :: Directives,
    etEnumValuesDefinition :: [EnumValueDefinition]
  }
  deriving (Show, Eq)

data EnumValueDefinition = EnumValueDefinition
  { evDescription :: Maybe Description,
    evEnumValue :: EnumValue,
    evDirectives :: Directives
  }
  deriving (Show, Eq)

type FieldsDefinition = [FieldDefinition]

data FieldDefinition = FieldDefinition
  { fdDescription :: Maybe Description,
    fdName :: Name,
    fdArgument :: ArgumentsDefinition,
    fdType :: GQLType,
    fdDirectives :: Directives
  }
  deriving (Show, Eq)

type ArgumentsDefinition = [InputValueDefinition]

type DefaultValue = Maybe Value

data InputValueDefinition = InputValueDefinition
  { adDescription :: Maybe Description,
    adName :: Name,
    adType :: GQLType,
    adDefaultValue :: DefaultValue,
    adDirectives :: Directives
  }
  deriving (Show, Eq)
