{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- AST
-}

module Ast.Type (Ast (..)) where

data Ast
  = Truth Bool -- Single known boolean value
  | If Ast Ast Ast -- branching condition
  deriving (Show, Eq)
