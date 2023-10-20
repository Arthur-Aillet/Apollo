--
-- EPITECH PROJECT, 2023
-- apollo
-- File description:
-- Values
--

module Eval.Values (
    Builtin (..), Value (..)
) where

data Builtin
  = Add
  | Sub
  | Mul
  | Div
  | Eq
  | Less
  deriving (Show)

data Value
  = Int Int
  | Bool Bool
  | Op Builtin
  | Func String
  deriving (Show)
