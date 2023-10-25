{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- AST To Insts Contxt
-}

module Ast.Context (Index(..), Context(..), LocalContext(..), createCtx, createLocalContext) where

import Data.HashMap.Lazy (HashMap, insert, fromList)
import Ast.Type
  ( Function (..),
    Type (TypeInt),
    Definition(..),
  )

newtype Index = Index Int

newtype Context = Context (HashMap String (Index, Function))

data LocalContext = LocalContext (HashMap String (Index, Type)) (Maybe Type)

attachIndex :: [(String, Type)] -> Index -> [(String, (Index, Type))]
attachIndex [] _ = []
attachIndex ((str, t) : xs) (Index acc) = (str, (Index acc, t)) : attachIndex xs (Index (acc + 1))

createCtx :: [Definition] -> Context -> Int -> Either String Context
createCtx (FuncDefinition string def : xs) (Context ctx) nbr =
  createCtx xs (Context $ insert string (Index nbr, def) ctx) (nbr + 1)
createCtx (VarDefinition _ _ : _) _ _ = Left "Error: Global Variables not supported yet"
createCtx [] ctx _ = Right ctx

createLocalContext :: [(String, Type)] -> Maybe Type -> Either String LocalContext
createLocalContext args output = Right $ LocalContext (fromList (attachIndex args (Index 0))) output
