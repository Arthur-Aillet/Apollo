{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- AST Compilation
-}

module Ast.Compile (module Ast.Compile) where

import Ast.Context (Context (..), CurrentReturnType, LocalContext (..), Variables, createCtx, createLocalContext, firstValidIndex)
import Ast.Error (Compile (..), withW)
import Ast.Operable (compOperable, compOperation, concatInner)
import Ast.Type
  ( Ast (..),
    Definition (..),
    Function (..),
    Operable (..),
    Structure (..),
    Type (..),
  )
import Data.HashMap.Lazy (adjust, empty, insert, member, (!?))
import Eval.Exec

data Binary = Binary Env Func deriving (Show)

generateBinary :: [Definition] -> Compile Binary
generateBinary defs = case createCtx defs (Context empty) 0 of
  Ko warns err -> Ko warns err
  Ok warns ctx -> case compAllFunc defs (Binary [] []) ctx of
    Ko warns2 err -> withW warns $ Ko warns2 err
    Ok warns2 (Binary _ []) -> withW warns $ Ko warns2 "no main function found"
    other -> other

compAllFunc :: [Definition] -> Binary -> Context -> Compile Binary
compAllFunc ((FuncDefinition "main" func) : xs) (Binary env []) ctx =
  case compFunc func ctx of
    Ko warns err -> Ko warns err
    Ok w function -> withW w $ compAllFunc xs (Binary env function) ctx
compAllFunc ((FuncDefinition _ (Function args y z)) : xs) (Binary env funcs) c =
  case compFunc (Function args y z) c of
    Ko warns err -> Ko warns err
    Ok w f ->
      withW w $ compAllFunc xs (Binary (env ++ [(length args, f)]) funcs) c
compAllFunc [] bin _ = Ok [] bin

compFunc :: Function -> Context -> Compile Insts
compFunc (Function args output ast) ctx = case createLocalContext args output of
  Ko warns err -> Ko warns err
  Ok w local -> withW w $ fst <$> compAst ast ctx local

compAst :: Ast -> Context -> LocalContext -> Compile (Insts, LocalContext)
compAst (AstStructure struct) c l = compStruct struct c l
compAst (AstOperation op) c l =
  (\a -> (a, l)) <$> (fst <$> compOperation op c l)

compIfOp :: Operable -> Context -> LocalContext -> (Compile Insts, Int)
compIfOp op c l = case compOperable op c l of
  Ko w e -> (Ko w e, 0)
  Ok w (op_insts, TypeBool) -> (Ok w op_insts, length op_insts)
  Ok w (_, op_type) ->
    ( Ko w $
        "If contain invalid type \""
          ++ show op_type
          ++ "\" instead of Bool",
      0
    )

compIfAst :: Ast -> Context -> LocalContext -> (Compile Insts, Int)
compIfAst ast c l = case compAst ast c l of
  Ko w e -> (Ko w e, 0)
  Ok w (insts, _) -> (Ok w insts, length insts)

compIf :: [(Operable, Ast)] -> Maybe Ast -> Context -> LocalContext -> (Compile Insts, Int)
compIf ((op, then') : xs) e c l =
  (concatInner [op_i, j_f, ast_i, jump_n, next_i], ast_len + op_len + next_len)
  where
    j_f = Ok [] [JumpIfFalse (ast_len + plus_one)]
    plus_one = if next_len == 0 then 0 else 1
    jump_n = if next_len == 0 then Ok [] [] else Ok [] [Jump next_len]
    (next_i, next_len) = compIf xs e c l
    (op_i, op_len) = compIfOp op c l
    (ast_i, ast_len) = compIfAst then' c l
compIf [] (Just else') c l = compIfAst else' c l
compIf [] Nothing _ _ = (Ok [] [], 0)

compVarDefinition :: Operable -> Variables -> Maybe Type -> Type -> String -> Context -> Compile (Insts, LocalContext)
compVarDefinition op hmap r vtype name c =
  case compOperable op c (LocalContext hmap r) of
    Ko warns err -> Ko warns err
    Ok w (insts, op_type)
      | op_type == vtype -> Ok w (insts ++ [Store], new_local)
      | otherwise -> Ko w $ "Type of variable \"" ++ name ++ "\" redefined"
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
      | otherwise -> Ko w $ "Type of variable \"" ++ name ++ "\" redefined"

compStruct :: Structure -> Context -> LocalContext -> Compile (Insts, LocalContext)
compStruct Resolved _ l = Ok [] ([], l)
compStruct (Return _) _ (LocalContext _ Nothing) =
  Ko [] "Return value in void function"
compStruct (Return ope) c (LocalContext a (Just fct_type)) =
  case compOperable ope c (LocalContext a (Just fct_type)) of
    Ko warns err -> Ko warns err
    Ok w (op_compiled, op_type)
      | op_type == fct_type ->
          Ok w (op_compiled ++ [Ret], LocalContext a (Just fct_type))
      | otherwise -> Ko w "Return invalid type"
compStruct (If ops else') c l = (\a -> (a, l)) <$> fst (compIf ops else' c l)
compStruct (Single _) _ _ = Ko [] "Single unsupported"
compStruct (Block _ _) _ _ = Ko [] "Block unsupported"
compStruct (Sequence (x : xs)) c l = case compAst x c l of
  Ko warns err -> Ko warns err
  Ok w (insts, new_local) ->
    (\(a_i, _) (b_i, f_l) -> (a_i ++ b_i, f_l))
      <$> Ok w (insts, new_local)
      <*> compStruct (Sequence xs) c new_local
compStruct (Sequence []) _ l = Ok [] ([], l)
compStruct (VarDefinition name vtype content) c (LocalContext vs r)
  | name `member` vs = Ko [] $ "Variable \"" ++ name ++ "\"  already exist"
  | otherwise = case content of
      Nothing ->
        Ok
          []
          ( [],
            LocalContext (insert name (firstValidIndex vs, vtype, False) vs) r
          )
      Just op -> compVarDefinition op vs r vtype name c
compStruct (VarAssignation name op) c (LocalContext hmap r) =
  case hmap !? name of
    Nothing -> Ko [] $ "Variable \"" ++ name ++ "\" undefined"
    Just (idx, wtype, True) -> case compOperable op c (LocalContext hmap r) of
      Ko warns err -> Ko warns err
      Ok w (insts, rtype)
        | wtype == rtype -> Ok w (insts ++ [Assign idx], LocalContext hmap r)
        | otherwise -> Ko [] $ "Type of variable \"" ++ name ++ "\" redefined"
    Just (_, wtype, False) -> compFirstAssign op c hmap r wtype name
compStruct (While op ast) c l = case compOperable op c l of
  Ko warns err -> Ko warns err
  Ok w (op_i, TypeBool) -> case compAst ast c l of
    Ko warns err -> Ko warns err
    Ok w2 (t_i, _) -> withW w2 $ Ok w (op_i ++ jumpIfFalse ++ t_i ++ jump, l)
      where
        jumpIfFalse = [JumpIfFalse (length t_i + 1)]
        jump = [Jump ((length op_i + length t_i + 1) * (-1))]
  Ok w (_, op_type) ->
    Ko w $ "If wait boolean and not " ++ show op_type
