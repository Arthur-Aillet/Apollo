{-
-- EPITECH PROJECT, 2023
-- apollo
-- File description:
-- Builtins
-}

module Eval.Operator
  ( execOperator,
    operate,
    Operator (..),
    Stack,
    defsOp,
    OperatorDef (..),
    OperatorType (..),
  )
where

import Eval.Atom (Atom (..))

type Stack = [Atom]

type ArgsNbr = Int

data OperatorDef = OperatorDef ArgsNbr OperatorType

data OperatorType
  = Equality
  | Calculus
  deriving (Show, Eq)

data Operator
  = Addition
  | Subtraction
  | Multiplication
  | Division
  | Modulo
  | Eq
  | Less
  deriving (Show, Eq)

operate :: Operator -> ([Atom] -> Either String Atom)
operate Addition = Right . sum
operate Subtraction = \[a, b] -> Right (a - b)
operate Multiplication = Right . product
operate Division = \[a, b] ->
  if b /= 0
    then Right (a / b)
    else Left "Division by zero"
operate Modulo = \[a, b] ->
  if b /= 0
    then Right (mod a b)
    else Left "Modulo by zero"
operate Eq = \[a, b] -> Right $ AtomB $ a == b
operate Less = \[a, b] -> Right $ AtomB $ a < b

defsOp :: Operator -> OperatorDef
defsOp Addition = OperatorDef 2 Calculus
defsOp Subtraction = OperatorDef 2 Calculus
defsOp Multiplication = OperatorDef 2 Calculus
defsOp Division = OperatorDef 2 Calculus
defsOp Modulo = OperatorDef 2 Calculus
defsOp Eq = OperatorDef 2 Equality
defsOp Less = OperatorDef 2 Equality

execOperator :: Stack -> Operator -> Either String Stack
execOperator stack op =
  if length (take count stack) >= count
    then case operate op top of
      Right res -> Right (res : bottom)
      Left x -> Left x
    else Left "not enough arguments in the stack"
  where
    (top, bottom) = splitAt count stack
    (OperatorDef count _) = defsOp op
