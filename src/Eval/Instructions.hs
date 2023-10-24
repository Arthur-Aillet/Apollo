--
-- EPITECH PROJECT, 2023
-- apollo
-- File description:
-- Instructions
--

module Eval.Instructions (
    Instruction (..), Insts, moveForward, Register, Func
) where

import Eval.Builtins (Operator (..), )
import Eval.Atom (Atom (..))

type Register = Int
type Func = [Instruction];

data Instruction
  = PushD Atom
  | PushI Register
  | CallD String
  | CallI Register
  | Op Operator
  | JumpIfFalse Int
  | Ret
  deriving (Show)

type Insts = [Instruction];

moveForward :: Int -> Insts -> Either String Insts
moveForward 0 insts = Right insts
moveForward nb [] = Left ("Error: Jump too far (" ++ show nb ++ ")")
moveForward nb (_:xs) = moveForward (nb - 1) xs
