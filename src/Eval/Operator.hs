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

data Value
  = VAtom Atom
  | VList [Value]
  deriving (Eq, Show)

type Stack = [Value]

type ArgsNbr = Int

data OperatorDef = OperatorDef ArgsNbr OperatorType

data OperatorType
  = Equality -- [a] -> Bool
  | Logical -- [Bool] -> Bool
  | Calculus -- [a] -> a
  | Concatenation -- [[a]] -> [a]
  | Printing -- [[Char]] -> Nothing
  | Getting -- [[a], Int] -> a
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
  | Print
  | Concat
  | Get
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
defsOp Print = OperatorDef 1 Printing
defsOp Concat = OperatorDef 2 Concatenation
defsOp Get = OperatorDef 2 Getting

isAllAtoms :: [Value] -> Either String [Atom]
isAllAtoms (VAtom x : xs) = (x :) <$> isAllAtoms xs
isAllAtoms [] = Right []
isAllAtoms _ = Left "Not all primitives"

getElemErrorMessage :: Int -> Int -> String
getElemErrorMessage a b =
  "Error: Element ["
    ++ show a
    ++ "] asked outside list (lenght "
    ++ show b
    ++ ")"

getElem :: Int -> [a] -> Either String a
getElem i [] = Left $ "Error: [" ++ show i ++ "] on an empty list"
getElem nb list
  | nb >= length list = Left $ getElemErrorMessage nb (length list)
  | nb < 0 =
      Left $
        "Error: Get element at a negativ index : ["
          ++ show nb
          ++ "]"
  | otherwise = Right $ last $ take (nb + 1) list

execOperator :: Stack -> Operator -> IO (Either String Stack)
execOperator (x : y : xs) Get = case (x, y) of
  (VList list, VAtom (AtomI idx)) -> case getElem idx list of
    Left err -> return $ Left err
    Right val -> return $ Right $ val : xs
  _ -> return (Left "Concat take two lists")
execOperator (x : y : xs) Concat = case (x, y) of
  (VList list1, VList list2) -> return $ Right $ VList (list1 ++ list2) : xs
  _ -> return (Left "Concat take two lists")
execOperator (x : xs) Print = case x of
  (VList []) -> return (Right xs)
  (VList (VAtom (AtomC c _) : chars)) ->
    putStr [c] >> execOperator (VList chars : xs) Print
  _ -> return (Left "Print with non string")
execOperator [] Print = return $ Left "Print with empty stack"
execOperator stack op
  | length (take count stack) >= count = case isAllAtoms top of
      Left x -> return $ Left x
      Right top' -> case operate op top' of
        Left x -> return $ Left x
        Right res -> return $ Right (VAtom res : bottom)
  | otherwise = return $ Left "not enough arguments in the stack"
  where
    (top, bottom) = splitAt count stack
    (OperatorDef count _) = defsOp op
