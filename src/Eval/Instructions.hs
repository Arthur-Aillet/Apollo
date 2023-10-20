--
-- EPITECH PROJECT, 2023
-- apollo
-- File description:
-- Instructions
--

module Eval.Instructions (
    Instruction (..), Insts, moveForward
) where

import Eval.Builtins (Builtin (..))
import Eval.Values (Value (..))

data Instruction
  = Push Value
  | PushArg Int
  | Call
  | JumpIfFalse Int
  | Ret
  deriving (Show)

type Insts = [Instruction];

moveForward :: Int -> Insts -> Either String Insts
moveForward 0 insts = Right insts
moveForward nb [] = Left ("Error: Jump too far (" ++ show nb ++ ")")
moveForward nb (_:xs) = moveForward (nb - 1) xs
