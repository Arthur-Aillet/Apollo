module Eval.Builtins (
  execOperator,
  Operator (..),
  Stack
) where

import Eval.Atom (Atom (..))

type Stack = [Atom];

data Operator
    = Addition
    | Subtraction
    | Multiplication
    | Division
    | Eq
    | Less
    deriving (Show, Eq)

operate :: Operator -> ([Atom] -> Either String Atom)
operate Addition = Right . sum
operate Subtraction = \ [a,b] -> Right (a - b)
operate Multiplication = Right . product
operate Division = \ [a,b] -> if b /= 0
                then Right (a / b)
                else Left "Division by zero"
operate Eq = \[a, b] -> Right $ AtomB $ a == b
operate less = \[a, b] -> Right $ AtomB $ a < b

operatorArgCount :: Operator -> Int
operatorArgCount Addition        = 2
operatorArgCount Subtraction     = 2
operatorArgCount Multiplication  = 2
operatorArgCount Division        = 2
operatorArgCount Eq              = 2
operatorArgCount Less            = 2

execOperator :: Stack -> Operator -> Either String Stack
execOperator stack op = if length (take count stack) >= count
        then case operate op top of
            Right res -> Right (res : bottom)
            Left x -> Left x
        else Left "not enough arguments in the stack"
    where
        (top, bottom) = splitAt count stack
        count = operatorArgCount op
