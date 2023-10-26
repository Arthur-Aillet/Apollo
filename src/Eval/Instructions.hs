--
-- EPITECH PROJECT, 2023
-- apollo
-- File description:
-- Instructions
--

module Eval.Instructions
  ( Instruction (..),
    Insts,
    moveForward,
    Index,
    Func,
  )
where

import Eval.Atom (Atom (..))
import Eval.Builtins (Operator (..))

type Index = Int

type Func = [Instruction]

data Instruction
  = PushD Atom
  | PushI Index
  | CallD Index
  | CallI Index
  | Op Operator
  | JumpIfFalse Int
  | Ret
  deriving (Show, Eq)

type Insts = [Instruction]

moveForward :: Int -> Insts -> Either String Insts
moveForward 0 insts = Right insts
moveForward nb [] = Left ("Error: Jump too far (" ++ show nb ++ ")")
moveForward nb (_ : xs) = moveForward (nb - 1) xs
