{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- Types
-}

module Ast.Type (module Ast.Type) where

data Type
  = TypeBool
  | TypeChar
  | TypeInt
  | TypeFloat
  | -- |  TypeBroken
    TypeList (Maybe Type)
  deriving (Eq)
