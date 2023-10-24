--
-- EPITECH PROJECT, 2023
-- apollo
-- File description:
-- Values
--

module Eval.Values (
    Builtin (..), Value (..)
) where

import Eval.Atom

data Builtin
  = Add
  | Sub
  | Mul
  | Div
  | Eq
  | Less
  deriving (Show)

data Value
  = Atom Atom
  | Op Builtin
  | Func String
  deriving (Show)
