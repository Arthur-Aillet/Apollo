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
  )
where

import Eval.Operator (Operator (..), Value)

type Index = Int

type Func = [Instruction]

data Instruction
  = PushD Value
  | Store
  | Take Int -- Make a list from Int values
  | Assign Index
  | PushI Index
  | CallD Index
  | CallI Index
  | Op Operator
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
