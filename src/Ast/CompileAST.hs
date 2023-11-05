{-
-- EPITECH PROJECT, 2023
-- apollo
-- File description:
-- AST Compilation
-}

module Ast.CompileAST (module Ast.CompileAST) where

import Ast.Ast
  ( Ast (..),
    Definition (..),
    Function (..),
  )
import Ast.CompileStruct
import Ast.Context (Context (..), LocalContext (..), createCtx, createLocalContext)
import Ast.Error (Compile (..), failingComp, withW)
import Ast.Operable (compOperation)
import Data.HashMap.Lazy (empty)
import Eval.Exec

mainErr :: [String]
mainErr = ["Main function missing"]

generateBinary :: [Definition] -> Compile Env
generateBinary defs = case createCtx defs (Context empty, False) 1 of
  Ko warns err -> Ko warns err
  Ok warns (ctx, False) ->
    failingComp (compAllFunc defs [] ctx) warns mainErr
  Ok warns (ctx, True) -> case compAllFunc defs [] ctx of
    Ko warns2 err -> withW warns $ Ko warns2 err
    other -> other

compAllFunc :: [Definition] -> Env -> Context -> Compile Env
compAllFunc ((FuncDefinition "main" (Function args y z)) : xs) env ctx =
  case compFunc (Function args y z) ctx of
    Ko warns err -> failingComp (compAllFunc xs env ctx) warns err
    Ok w f -> withW w $ compAllFunc xs ((length args, f) : env) ctx
compAllFunc ((FuncDefinition _ (Function args y z)) : xs) env c =
  case compFunc (Function args y z) c of
    Ko warns err -> Ko warns err
    Ok w f ->
      withW w $ compAllFunc xs (env ++ [(length args, f)]) c
compAllFunc [] bin _ = Ok [] bin

compFunc :: Function -> Context -> Compile Insts
compFunc (Function args output ast) ctx =
  fst <$> compAst ast ctx (createLocalContext args output)

compAst :: Ast -> Context -> LocalContext -> Compile (Insts, LocalContext)
compAst (AstStructure struct) c l = compStruct compAst struct c l
compAst (AstOperation op) c l =
  (\a -> (a, l)) <$> (fst <$> compOperation op c l)
