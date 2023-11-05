{-
-- EPITECH PROJECT, 2023
-- apollo
-- File description:
-- AST-struct Compilation
-}

module Ast.CompileStruct (module Ast.CompileStruct) where

import Ast.Ast
  ( Structure (..),
    Type (..),
  )
import Ast.CompileArray
import Ast.CompileFor
import Ast.CompileIf
import Ast.CompileVar
import Ast.Context (Compiler, Context (..), LocalContext (..), firstValidIndex)
import Ast.Error (Compile (..), failingComp, withW)
import Ast.Operable (compOperable)
import Data.HashMap.Lazy (insert, member, (!?))
import Eval.Exec

compStruct :: Compiler -> Structure -> Context -> LocalContext -> Compile (Insts, LocalContext)
compStruct _ Resolved _ l = Ok [] ([], l)
compStruct _ (Return _) _ (LocalContext _ Nothing) =
  Ko [] ["Return value in void function"]
compStruct _ (Return ope) c (LocalContext a (Just fct_type)) =
  case compOperable ope c (LocalContext a (Just fct_type)) of
    Ko warns err -> Ko warns err
    Ok w (op_compiled, op_type)
      | op_type == fct_type ->
          Ok w (op_compiled ++ [Ret], LocalContext a (Just fct_type))
      | otherwise -> Ko w ["Return invalid type"]
compStruct compiler (If ops else') c l =
  (\a -> (a, l)) <$> fst (compIf compiler ops else' c l)
compStruct _ (Single _) _ _ = Ko [] ["Single unsupported"]
compStruct _ (Block _ _) _ _ = Ko [] ["Block unsupported"]
compStruct compiler (Sequence (x : xs)) c l = case compiler x c l of
  Ko warns1 err1 ->
    failingComp (compStruct compiler (Sequence xs) c l) warns1 err1
  Ok w (insts, new_local) ->
    (\(a_i, _) (b_i, f_l) -> (a_i ++ b_i, f_l))
      <$> Ok w (insts, new_local)
      <*> compStruct compiler (Sequence xs) c new_local
compStruct _ (Sequence []) _ l = Ok [] ([], l)
compStruct _ (VarDefinition name vtype content) c (LocalContext vs r)
  | name `member` vs = Ko [] ["Variable \"" ++ name ++ "\"  already exist"]
  | otherwise = case content of
      Nothing ->
        Ok
          []
          ( [],
            LocalContext (insert name (firstValidIndex vs, vtype, False) vs) r
          )
      Just op -> compVarDefinition op vs r vtype name c
compStruct _ (VarAssignation name op) c (LocalContext hmap r) =
  case hmap !? name of
    Nothing -> Ko [] ["Variable \"" ++ name ++ "\" undefined"]
    Just (idx, wtype, True) -> case compOperable op c (LocalContext hmap r) of
      Ko warns err -> Ko warns err
      Ok w (insts, rtype)
        | wtype == rtype -> Ok w (insts ++ [Assign idx], LocalContext hmap r)
        | otherwise -> Ko [] ["Type of variable \"" ++ name ++ "\" redefined1"]
    Just (_, wtype, False) -> compFirstAssign op c hmap r wtype name
compStruct _ (ArrAssignation name idx_ops val) ctx (LocalContext hmap r) =
  case hmap !? name of
    Nothing -> Ko [] ["Array \"" ++ name ++ "\" undefined"]
    Just (_, _, False) -> failingComp (compOperable val ctx l) [] message
    Just (idx, wtype, True) -> compArrAssignation idx_ops val idx wtype ctx l
  where
    message = ["Array \"" ++ name ++ "\" undeclared"]
    l = LocalContext hmap r
compStruct compiler (While op ast) c l = case compOperable op c l of
  Ko warns err -> failingComp (compiler ast c l) warns err
  Ok w (op_i, TypeBool) -> case compiler ast c l of
    Ko warns err -> Ko warns err
    Ok w2 (t_i, _) -> withW w2 $ Ok w (op_i ++ jumpIfFalse ++ t_i ++ jump, l)
      where
        jumpIfFalse = [JumpIfFalse (length t_i + 1)]
        jump = [Jump ((length op_i + length t_i + 1) * (-1))]
  Ok w (_, op_type) ->
    Ko w ["While condition await boolean and not " ++ show op_type]
compStruct compiler (For iter_name arr ast) ctx l =
  compFor compiler iter_name arr ast ctx l
