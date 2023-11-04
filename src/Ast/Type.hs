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

instance Show Type where
  show TypeBool = "bool"
  show TypeChar = "char"
  show TypeInt = "int"
  show TypeFloat = "float"
  show (TypeList (Just type')) = "[" ++ show type' ++ "]"
  show (TypeList Nothing) = "[]"

numType :: Type -> Bool
numType TypeBool = True
numType TypeChar = True
numType TypeInt = True
numType TypeFloat = True
numType _ = False
