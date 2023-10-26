{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- AST To Insts Contxt
-}

module Ast.Context (Index, Context (..), LocalContext (..), createCtx, createLocalContext) where

import Ast.Type
  ( Definition (..),
    Function (..),
    Type,
  )
import Data.HashMap.Lazy (HashMap, fromList, insert)

type Index = Int

newtype Context = Context (HashMap String (Index, [(String, Type)], Maybe Type))

data LocalContext = LocalContext (HashMap String (Index, Type)) (Maybe Type)

attachIndex :: [(String, Type)] -> Index -> [(String, (Index, Type))]
attachIndex [] _ = []
attachIndex ((str, t) : xs) acc = (str, (acc, t)) : attachIndex xs (acc + 1)

createCtx :: [Definition] -> Context -> Int -> Either String Context
createCtx (FuncDefinition name (Function args rval _) : xs) (Context ctx) nbr =
  createCtx xs (Context $ insert name (nbr, args, rval) ctx) (nbr + 1)
createCtx (VarDefinition _ _ : _) _ _ =
  Left "Error: Global Variables not supported yet"
createCtx [] ctx _ = Right ctx

createLocalContext :: [(String, Type)] -> Maybe Type -> Either String LocalContext
createLocalContext args output =
  Right $ LocalContext (fromList (attachIndex args 0)) output
