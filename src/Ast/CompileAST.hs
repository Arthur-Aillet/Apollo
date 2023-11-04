{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- AST Compilation
-}

module Ast.CompileAST (module Ast.CompileAST) where

import Ast.CompileStruct
import Ast.Context (Context (..), LocalContext (..), createCtx, createLocalContext)
import Ast.Error (Compile (..), failingComp, withW)
import Ast.Operable (compOperation)
import Ast.Type
  ( Ast (..),
    Definition (..),
    Function (..),
  )
import Data.HashMap.Lazy (empty)
import Eval.Exec

data Binary = Binary Env Func deriving (Show)

generateBinary :: [Definition] -> Compile Binary
generateBinary defs = case createCtx defs (Context empty) 0 of
  Ko warns err -> Ko warns err
  Ok warns ctx -> case compAllFunc defs (Binary [] []) ctx of
    Ko warns2 err -> withW warns $ Ko warns2 err
    Ok warns2 (Binary _ []) ->
      withW warns $ Ko warns2 ["No main function found"]
    other -> other

compAllFunc :: [Definition] -> Binary -> Context -> Compile Binary
compAllFunc ((FuncDefinition "main" func) : xs) (Binary env []) ctx =
  case compFunc func ctx of
    Ko warns err -> failingComp (compAllFunc xs (Binary env []) ctx) warns err
    Ok w function -> withW w $ compAllFunc xs (Binary env function) ctx
compAllFunc ((FuncDefinition _ (Function args y z)) : xs) (Binary env funcs) c =
  case compFunc (Function args y z) c of
    Ko warns err -> Ko warns err
    Ok w f ->
      withW w $ compAllFunc xs (Binary (env ++ [(length args, f)]) funcs) c
compAllFunc [] bin _ = Ok [] bin

compFunc :: Function -> Context -> Compile Insts
compFunc (Function args output ast) ctx =
  fst <$> compAst ast ctx (createLocalContext args output)

compAst :: Ast -> Context -> LocalContext -> Compile (Insts, LocalContext)
compAst (AstStructure struct) c l = compStruct compAst struct c l
compAst (AstOperation op) c l =
  (\a -> (a, l)) <$> (fst <$> compOperation op c l)
