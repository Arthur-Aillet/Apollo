{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- AST
-}

module Ast.Type (Ast (..), Function (..), Structure (..), Operation (..), Type (..), Definition (..), Operable (..), numType, atomType, valueType) where

import Eval.Atom (Atom (..))
import Eval.Operator (Operator, Value (..))

data Function = Function [(String, Type)] (Maybe Type) Ast deriving (Show, Eq)

data Definition
  = FuncDefinition String Function -- define a function

data Structure -- layout, structure and connection of statements, having no value
  = Resolved -- expression resolving to no value
  | VarDefinition String Type (Maybe Operable)
  | VarAssignation String Operable
  | Return Operable
  | If [(Operable, Ast)] (Maybe Ast) -- branching condition (if ((x) {}, ...) {})
  | While Operable Ast
  | Single Ast -- single operation or operable ({x})
  | Block [Ast] [String] -- several actions ordered by variable precedence ({x;y})
  | Sequence [Ast] -- several actions ordered by precedence ({x >> y})
  deriving (Show, Eq)

data Operation -- statement involving an action, resulting in a value
  = Interrupt String -- Interrupt program flow
  | CallStd Operator [Operable] -- call a standard or operator operation (x(y))
  | CallFunc String [Operable] -- call a function, exposes both inherent IOPipes (x(y))
  | CallSH String [Operable] -- syscall of Operator program ($x(y)), exposes both IOPipes
  | Pipe Operable Operable -- stdout mapped to stdin ({x.y}, {x <- y})
  deriving (Show, Eq)

data Operable -- statement having a value
  = OpVariable String -- Variable reffering to single known value
  | OpCast Operable Type
  | OpValue Atom -- Single known value
  | OpList [Operable]
  | OpOperation Operation -- operation resulting in an operable value
  | OpIOPipe String -- named pipe, String is likely a placeholder
  deriving (Show, Eq)

data Type
  = TypeBool
  | TypeChar
  | TypeInt
  | TypeFloat
  | TypeList (Maybe Type)
  deriving (Eq)

instance Show Type where
  show TypeBool = "bool"
  show TypeChar = "char"
  show TypeInt = "int"
  show TypeFloat = "float"
  show (TypeList (Just type')) = "[" ++ show type' ++ "]"
  show (TypeList Nothing) = "[]"

numType :: Type -> Bool
numType TypeBool = True
numType TypeChar = True
numType TypeInt = True
numType TypeFloat = True
numType _ = False

valueType :: Value -> Type
valueType (VAtom a) = atomType a
valueType (VList []) = TypeList Nothing
valueType (VList (x : _)) = TypeList $ Just $ valueType x

atomType :: Atom -> Type
atomType (AtomB _) = TypeBool
atomType (AtomC _ _) = TypeChar
atomType (AtomI _) = TypeInt
atomType (AtomF _) = TypeFloat

data Ast
  = AstStructure Structure -- structure block ({})
  | AstOperation Operation -- take an action
  deriving (Show, Eq)
