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
    Value (..),
    Stack,
    defsOp,
    OperatorDef (..),
    OperatorType (..),
  )
where

import Eval.Atom (Atom (..))
import Data.Bits (And)

data Value
  = VAtom Atom
  deriving
    ( -- | List [Atom]
      Show,
      Eq
    )

type Stack = [Value]

type ArgsNbr = Int

data OperatorDef = OperatorDef ArgsNbr OperatorType

data OperatorType
  = Equality -- [a] -> Bool
  | Logical -- [Bool] -> Bool
  | Calculus -- [a] -> a
  deriving (Show, Eq)

data Operator
  = Add
  | Sub
  | Mul
  | Div
  | Mod
  | Eq
  | Lt
  | LEt
  | Gt
  | GEt
  | NEq
  | And
  | Or
  | Not
  deriving (Show, Eq)

operate :: Operator -> ([Atom] -> Either String Atom)
operate Add = Right . sum
operate Sub = \[a, b] -> Right (a - b)
operate Mul = Right . product
operate Div = \[a, b] ->
  if b /= 0
    then Right (a / b)
    else Left "Division by zero"
operate Mod = \[a, b] ->
  if b /= 0
    then Right (a `mod` b)
    else Left "Modulo by zero"
operate Eq = \[a, b] -> Right $ AtomB $ a == b
operate Lt = \[a, b] -> Right $ AtomB $ a < b
operate Gt = \[a, b] -> Right $ AtomB $ a > b
operate GEt = \[a, b] -> Right $ AtomB $ a >= b
operate GEt = \[a, b] -> Right $ AtomB $ a >= b
operate NEq = \[a, b] -> Right $ AtomB $ a /= b
operate And = \[AtomB a, AtomB b] -> Right $ AtomB $ a && b
operate Or = \[AtomB a, AtomB b] -> Right $ AtomB $ a || b
operate Not = \[AtomB a] -> Right $ AtomB $ not a

defsOp :: Operator -> OperatorDef
defsOp Add = OperatorDef 2 Calculus
defsOp Sub = OperatorDef 2 Calculus
defsOp Mul = OperatorDef 2 Calculus
defsOp Div = OperatorDef 2 Calculus
defsOp Mod = OperatorDef 2 Calculus
defsOp Eq = OperatorDef 2 Equality
defsOp Lt = OperatorDef 2 Equality
defsOp LEt = OperatorDef 2 Equality
defsOp Gt = OperatorDef 2 Equality
defsOp GEt = OperatorDef 2 Equality
defsOp NEq = OperatorDef 2 Equality
defsOp And = OperatorDef 2 Logical
defsOp Or = OperatorDef 2 Logical
defsOp Not = OperatorDef 1 Logical

isAllAtoms :: [Value] -> Either String [Atom]
isAllAtoms (VAtom x : xs) = (x :) <$> isAllAtoms xs
isAllAtoms [] = Right []
isAllAtoms _ = Left "Not all primitives"

execOperator :: Stack -> Operator -> Either String Stack
execOperator stack op
  | length (take count stack) >= count = case isAllAtoms top of
      Left x -> Left x
      Right top' -> case operate op top' of
        Left x -> Left x
        Right res -> Right (VAtom res : bottom)
  | otherwise = Left "not enough arguments in the stack"
  where
    (top, bottom) = splitAt count stack
    (OperatorDef count _) = defsOp op
