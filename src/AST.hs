{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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
    oName :: Maybe Name,
    oVariableDefinitons :: [VariableDefinition],
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

data Selection = SelectionField Field
  deriving (Show, Eq)

data Field = Field
  { fname :: Name,
    fArguments :: [Argument],
    fselectionSet :: SelectionSet
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

-- Types
data GQLType = NamedType Name | ListType GQLType | NonNullType NonNullGQLType
  deriving (Show, Eq)

data NonNullGQLType = NonNullNamedType Name | NonNullListType GQLType
  deriving (Show, Eq)