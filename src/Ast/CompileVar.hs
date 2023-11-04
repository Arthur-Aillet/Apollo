{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- variable Compilation
-}

module Ast.CompileVar (module Ast.CompileVar) where

import Ast.Ast
  ( Operable (..),
    Type (..),
  )
import Ast.Context (Context (..), CurrentReturnType, LocalContext (..), Variables, firstValidIndex)
import Ast.Error (Compile (..), Error)
import Ast.Operable (compOperable)
import Data.HashMap.Lazy (adjust, insert)
import Eval.Exec

compVarErr :: Type -> Type -> String -> [Error]
compVarErr op_type vtype name =
  [ "Type of variable \""
      ++ name
      ++ "\" redefined from "
      ++ show op_type
      ++ " to "
      ++ show vtype
  ]

compVarDefinition :: Operable -> Variables -> Maybe Type -> Type -> String -> Context -> Compile (Insts, LocalContext)
compVarDefinition op hmap r vtype name c =
  case compOperable op c (LocalContext hmap r) of
    Ko warns err -> Ko warns err
    Ok w (insts, TypeList Nothing) -> Ok w (insts ++ [Store], new_local)
    Ok w (insts, op_type)
      | op_type == vtype -> Ok w (insts ++ [Store], new_local)
      | otherwise -> Ko w $ compVarErr op_type vtype name
  where
    new_local = LocalContext new_hmap r
    new_hmap = insert name (firstValidIndex hmap, vtype, True) hmap

setTrue :: (Index, Type, Bool) -> (Index, Type, Bool)
setTrue (a, b, _) = (a, b, True)

compFirstAssign :: Operable -> Context -> Variables -> CurrentReturnType -> Type -> String -> Compile (Insts, LocalContext)
compFirstAssign op c hmap r wtype name =
  case compOperable op c (LocalContext hmap r) of
    Ko warns err -> Ko warns err
    Ok w (insts, rtype)
      | wtype == rtype ->
          Ok w (insts ++ [Store], LocalContext (adjust setTrue name hmap) r)
      | otherwise -> Ko w ["Type of variable \"" ++ name ++ "\" redefined2"]
