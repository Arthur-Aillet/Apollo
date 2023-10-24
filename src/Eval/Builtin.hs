module Eval.Builtin (module Eval.Builtin) where

import Data.HashMap.Lazy (HashMap)

data Value
  = Int Int
  | Bool Bool
  | Op Builtin
  | Func String
  deriving (Show)

data Instruction
  = Push Value
  | PushArg Int
  | Call
  | JumpIfFalse Int
  | Ret
  deriving (Show)

data Builtin
  = Add
  | Sub
  | Mul
  | Div
  | Eq
  | Less
  deriving (Show)

type Args = [Value]

type Stack = [Value]

type Insts = [Instruction]

type Env = (HashMap String (Int, Insts))

type Func = [Instruction]

moveForward :: Int -> Insts -> Either String Insts
moveForward 0 insts = Right insts
moveForward nb [] = Left ("Error: Jump too far (" ++ show nb ++ ")")
moveForward nb (_ : xs) = moveForward (nb - 1) xs

execBuiltin :: Builtin -> Stack -> Either String Stack
execBuiltin _ [] = Left "Op on empty stack"
execBuiltin Add (x : y : xs) = case (x, y) of
  (Int ix, Int iy) -> Right (Int (ix + iy) : xs)
  _ -> Left "Error: Add on unsupported types"
execBuiltin Sub (x : y : xs) = case (x, y) of
  (Int ix, Int iy) -> Right (Int (ix - iy) : xs)
  _ -> Left "Error: Sub on unsupported types"
execBuiltin Mul (x : y : xs) = case (x, y) of
  (Int ix, Int iy) -> Right (Int (ix * iy) : xs)
  _ -> Left "Error: Mul on unsupported types"
execBuiltin Div (x : y : xs) = case (x, y) of
  (Int _, Int 0) -> Left "Error : division by 0"
  (Int ix, Int iy) -> Right (Int (div ix iy) : xs)
  _ -> Left "Error: Div on unsupported types"
execBuiltin Eq (x : y : xs) = case (x, y) of
  (Int ix, Int iy) -> Right (Bool ((==) ix iy) : xs)
  (Bool bx, Bool by) -> Right (Bool ((==) bx by) : xs)
  _ -> Left "Error: Eq on unsupported types"
execBuiltin Less (x : y : xs) = case (x, y) of
  (Int ix, Int iy) -> Right (Bool ((<) ix iy) : xs)
  _ -> Left "Error: Eq on unsupported types"
execBuiltin _ _ = Left "Op on only one element"
