{-
-- EPITECH PROJECT, 2023
-- apollo
-- File description:
-- AST To Insts Contxt
-}

module Ast.Context (Compiler, Index, Context (..), LocalContext (..), createCtx, createLocalContext, firstValidIndex, Variables, Defined, CurrentReturnType) where

import Ast.Ast
  ( Ast (..),
    Definition (..),
    Function (..),
    Type,
  )
import Ast.Error (Compile (..))
import Data.HashMap.Lazy (HashMap, fromList, insert)
import Eval.Instructions (Insts)

type Index = Int

newtype Context = Context (HashMap String (Index, [(String, Type)], Maybe Type))

type CurrentReturnType = (Maybe Type)

type Defined = Bool

type Variables = (HashMap String (Index, Type, Defined))

data LocalContext = LocalContext Variables CurrentReturnType

type Compiler = (Ast -> Context -> LocalContext -> Compile (Insts, LocalContext))

attachIndex :: [(String, Type)] -> Index -> [(String, (Index, Type, Defined))]
attachIndex [] _ = []
attachIndex ((str, t) : xs) acc =
  (str, (acc, t, True)) : attachIndex xs (acc + 1)

createCtx :: [Definition] -> (Context, Bool) -> Int -> Compile (Context, Bool)
createCtx (FuncDefinition "main" (Function a rval _) : xs) (Context ctx, _) n =
  createCtx xs (Context $ insert "main" (0, a, rval) ctx, True) n
createCtx (FuncDefinition name (Function a rval _) : xs) (Context ctx, bool) n =
  createCtx xs (Context $ insert name (n, a, rval) ctx, bool) (n + 1)
createCtx [] ctx _ = Ok [] ctx

createLocalContext :: [(String, Type)] -> Maybe Type -> LocalContext
createLocalContext args = LocalContext (fromList (attachIndex args 0))

firstValidIndex :: Variables -> Index
firstValidIndex vars = foldl (\var (idx, _, _) -> max var idx) (-1) vars + 1
