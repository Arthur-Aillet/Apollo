{-
-- EPITECH PROJECT, 2023
-- apollo
-- File description:
-- Instructions
-}

module Eval.Instructions
  ( Instruction (..),
    Insts,
    moveForward,
    Index,
    Func,
    History,
    Machine,
    Pointer (..),
    Env,
    Args,
  )
where

import Ast.Type (Type (..))
import Eval.Atom (Atom (..))
import Eval.Operator
  ( Operator (..),
    Stack,
    Value (..),
  )
import Eval.Syscall (Syscall (..))

type Index = Int

type Func = [Instruction]

data Instruction
  = PushD Atom
  | Store
  | Take Int -- Make a list from Int values
  | Assign Index
  | ArrAssign Index -- arr[i] = val    (take VList of [1][2][..] and val)
  | PushI Index
  | CallD Index
  | CallI Index
  | Cast Type
  | CallS -- call SH Args Name
  | Op Operator
  | Sys Syscall
  | JumpIfFalse Int
  | Jump Int
  | Ret
  deriving (Show, Eq)

type Insts = [Instruction]

type History = [Instruction]

moveForward :: Int -> Insts -> Either String (Insts, Insts)
moveForward nb insts
  | nb < 0 = Left $ "Error: Jump before zero (" ++ show nb ++ ")"
  | nb > length insts = Left $ "Error: Jump too far (" ++ show nb ++ ")"
  | otherwise = Right $ splitAt nb insts

type Env = [(Int, Func)]

type Args = [Value]

data Pointer = Pointer Index [Index] deriving (Show)

type Machine = (Env, Args, Insts, History, Stack)
